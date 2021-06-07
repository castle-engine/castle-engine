// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleTransform" -*-
{
  Copyright 2012-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleTransform;

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry, CastleTestCase;

type
  TTestCastleTransform = class(TCastleTestCase)
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
    procedure TestNotificationsViewport;
    procedure TestList;
    procedure TestViewVectorsOrthogonal1;
    procedure TestViewVectorsOrthogonal2;
    procedure TestListNotification;
    procedure TestWorldFull;
    procedure TestWorldPrematureFree;
    procedure TestWorldFreeBeforeItem;
    procedure TestDirectionUp_UpYDirectionMinusZ;
    procedure TestDirectionUp_UpYDirectionZ;
    procedure TestTransformingScene;
    procedure TestPhysicsWorldOwner;
    procedure TestPhysicsWorldOwnerEmptyBox;
    procedure TestPhysicsWorldOwnerEmptySphere;
    procedure TestPhysicsExistsAfterParentTransformExistenceChange;
    procedure TestPass;
    procedure TestPassCombine;
    procedure TestForIn;
  end;

implementation

uses Math, Contnrs, Kraft,
  CastleVectors, CastleBoxes, CastleTransform, CastleViewport, CastleClassUtils,
  CastleTriangles, CastleSceneCore, X3DNodes, CastleScene, CastleInternalRenderer;

{ TMy3D ---------------------------------------------------------------------- }

type
  { Simple 3D axis-aligned box, resolving collisions with this box using
    TCastleScene fallback methods when Spatial = []. }
  TMy3D = class(TCastleSceneCore)
  private
    MyBox: TBox3D;
  public
    constructor Create(AOwner: TComponent; const AMyBox: TBox3D); reintroduce;
    function LocalBoundingBox: TBox3D; override;
    function Middle: TVector3; override;
  end;

constructor TMy3D.Create(AOwner: TComponent; const AMyBox: TBox3D);
var
  Root: TX3DRootNode;
  BoxTransform: TTransformNode;
  BoxShape: TShapeNode;
  Box: TBoxNode;
begin
  inherited Create(AOwner);
  MyBox := AMyBox;

  Box := TBoxNode.CreateWithTransform(BoxShape, BoxTransform);
  Box.Size := MyBox.Size;
  BoxTransform.Translation := MyBox.Center;

  Root := TX3DRootNode.Create;
  Root.AddChildren(BoxTransform);

  Load(Root, true);
end;

function TMy3D.LocalBoundingBox: TBox3D;
begin
  { Note that we don't check Collides property in BoundingBox method.
    BoundingBox must take into account also non-collidable
    (but visible) 3D object parts. (Well, in the case of this trivial
    3D object, it's actually never visible, so it's a little pointless...
    But in general, the rule is "BoundingBox looks at GetExists,
    not at Collides property".) }
  if GetExists then
    Result := MyBox
  else
    Result := TBox3D.Empty;
end;

function TMy3D.Middle: TVector3;
begin
  // do not require World.GravityCoordinate for Middle implementation
  Result := TVector3.Zero;
end;

{ Helper box values ---------------------------------------------------------- }

const
  Box0: TBox3D = (Data: (
    (Data: (-1, -1, -1)),
    (Data: (1, 1, 1))
  ));
  Box20: TBox3D = (Data: (
    (Data: (19, -1, -1)),
    (Data: (21, 1, 1))
  ));

{ TTestCastleTransform ---------------------------------------------------------------- }

procedure TTestCastleTransform.TestMy3D;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertSameValue(1, AboveHeight);

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertVectorEquals(Vector3(-2, -1.5, 0), NewPos);

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertSameValue(9, Collision.Distance);
    AssertVectorEquals(Collision.Last.Point, Vector3(1, 0, 0));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.TestMy3DNotExists;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    M.Exists := false;

    AssertTrue(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.TestMy3DNotCollides;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    M.Collides := false;

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertSameValue(9, Collision.Distance);
    AssertVectorEquals(Collision.Last.Point, Vector3(1, 0, 0));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

type
  { Define my own TCastleTransform descendant, only to expose OnlyTranslation
    value for testing. }
  TMy3DTransform = class(TCastleTransform)
  end;

procedure TTestCastleTransform.Test3DTransform;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertSameValue(1, AboveHeight);

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertVectorEquals(Vector3(-2, -1.5, 0), NewPos);

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertSameValue(9, Collision.Distance);
    AssertVectorEquals(Collision.Last.Point, Vector3(1, 0, 0));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.Test3DTransformNotExists;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Exists := false;
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.Test3DTransformNotCollides;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: PTriangle;
  NewPos: TVector3;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Collides := false;
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(2, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(-2, -2, 0), Vector3(-2, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3(10, 10, 10), Vector3(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(10, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(10, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertSameValue(9, Collision.Distance);
    AssertVectorEquals(Collision.Last.Point, Vector3(1, 0, 0));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.Test3DTransformReal;

  { Perform test on M, assuming that it results in box Box20. }
  procedure DoTests(M: TMy3DTransform);
  var
    IsAbove: boolean;
    AboveHeight: Single;
    AboveGround: PTriangle;
    NewPos: TVector3;
    Collision: TRayCollision;
  begin
    AssertTrue(M.BoundingBox.Equal(Box20));

    IsAbove := M.HeightCollision(Vector3(0.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(10.5, 10.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3(20.5, 0.5, 2), Vector3(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertSameValue(1, AboveHeight);

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(22, 2, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0), NewPos,
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0), NewPos,
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertVectorEquals(Vector3(18, -1.5, 0), NewPos);

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(22, 2, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0),
      true, 1.5, TBox3D.Empty, TBox3D.Empty, nil));
    AssertTrue(M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0),
      true, 0.5, TBox3D.Empty, TBox3D.Empty, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(22, 2, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(22, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(22, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3(18, -2, 0), Vector3(18, -1.5, 0),
      false, 0, TBox3D.Empty, Box3DAroundPoint(Vector3(22, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3(30, 10, 10), Vector3(40, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3(30, 10, 10), Vector3(10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3(22, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3(22, 2, 2), 3, nil));
    { below radius values chosen specifically to test that "/ AverageScale"
      inside TCastleTransform.SphereCollision works Ok }
    AssertTrue(not M.SphereCollision(Vector3(22, 0, 0), 0.9, nil));
    AssertTrue(M.SphereCollision(Vector3(22, 0, 0), 1.1, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3(22, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3(22, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3(30, 10, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3(30, 0, 0), Vector3(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertSameValue(9, Collision.Distance, 0.001); // larger epsilon for ppc64
    AssertVectorEquals(Collision.Last.Point, Vector3(21, 0, 0), 0.001);
    FreeAndNil(Collision);
  end;

var
  M, M2: TMy3DTransform;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3(20, 0, 0);
    AssertTrue(M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;

  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3(20, 0, 0);
    M.Rotation := Vector4(0, 1, 0, Pi / 2);
    AssertTrue(not M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;

  M := nil;
  M2 := nil;
  try
    M := TMy3DTransform.Create(nil);
    M2 := TMy3DTransform.Create(nil);

    { test rotation and translation in different order.
      This requires connecting 2 TCastleTransform instances, and using Center. }
    M.Add(M2);
    M.Rotation := Vector4(0, 1, 0, Pi / 2);
    M.Center := Vector3(20, 0, 0);
    AssertTrue(not M.OnlyTranslation);
    M2.Add(TMy3D.Create(M, Box0));
    M2.Translation := Vector3(20, 0, 0);
    AssertTrue(M2.OnlyTranslation);
    DoTests(M);
  finally
    FreeAndNil(M);
    FreeAndNil(M2);
  end;

  M := TMy3DTransform.Create(nil);
  try
    { use scaling in TCastleTransform }
    M.Add(TMy3D.Create(M, Box3D(
      Vector3(-0.25, -0.25, -0.25),
      Vector3( 0.25,  0.25,  0.25))));
    M.Translation := Vector3(20, 0, 0);
    M.Scale := Vector3(4, 4, 4);
    AssertTrue(not M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;
end;

procedure TTestCastleTransform.TestNotifications;
var
  ListParent, List: TCastleTransform;
  ItemOnList: TCastleTransform;
begin
  ListParent := TCastleTransform.Create(nil);
  try
    List := TCastleTransform.Create(ListParent);
    try
      ListParent.Add(List);

      ItemOnList := TCastleTransform.Create(ListParent);
      List.Add(ItemOnList);

      { now this will cause TCastleTransform.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(ListParent) end;

  ListParent := TCastleTransform.Create(nil);
  try
    List := TCastleTransform.Create(ListParent);
    try
      ListParent.Add(List);

      ItemOnList := TCastleTransform.Create(List);
      List.Add(ItemOnList);

      { now this will cause TCastleTransform.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(ListParent) end;
end;

procedure TTestCastleTransform.TestNotificationsViewport;
var
  Viewport: TCastleViewport;
  List: TCastleTransform;
  ItemOnList: TCastleTransform;
begin
  Viewport := TCastleViewport.Create(nil);
  try
    List := TCastleTransform.Create(Viewport);
    try
      Viewport.Items.Add(List);

      ItemOnList := TCastleTransform.Create(Viewport);
      List.Add(ItemOnList);

      { now this will cause TCastleTransform.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(Viewport) end;
end;

procedure TTestCastleTransform.TestList;
var
  My, My2: TCastleTransform;
  List: TCastleTransform;
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

    { Test that our TCastleTransformList avoids this bug:
      http://bugs.freepascal.org/view.php?id=21087 }

    List := TCastleTransform.Create(nil);
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

procedure TTestCastleTransform.TestViewVectorsOrthogonal1;
{ Test forcing Direction/Up orthogonal by various TCastleTransform routines
  (that actually implement it by TWalkCamera routines).
  This tests doesn't pass dir/up parallel. }
var
  O: TCastleTransform;
begin
  O := TCastleTransform.Create(nil);

  { no need to change direction/up angle, only normalize them }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(Vector3(0, 0, 1), O.Up, 0.1);

  O.SetView(Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(0, 0, 10));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(Vector3(0, 0, 1), O.Up, 0.1);

  { SetView corrects up vector angle }
  O.SetView(Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 0, 10));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(Vector3(0, 0, 1), O.Up, 0.1);

  { SetView with AdjustUp = false corrects direction vector angle }
  O.SetView(Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 0, 10), false);
  AssertVectorEquals(Vector3(Sqrt(2), 0, -Sqrt(2)).Normalize, O.Direction, 0.1);
  AssertVectorEquals(Vector3(Sqrt(2), 0, Sqrt(2)).Normalize, O.Up, 0.1);

  { Setting direction corrects up vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.Direction := Vector3(10, 0, 10);
  AssertVectorEquals(Vector3(Sqrt(2), 0, Sqrt(2)).Normalize, O.Direction, 0.1);
  AssertVectorEquals(Vector3(-Sqrt(2), 0, Sqrt(2)).Normalize, O.Up, 0.1);

  { Setting up corrects direction vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.Up := Vector3(10, 0, 10);
  AssertVectorEquals(Vector3(Sqrt(2), 0, -Sqrt(2)).Normalize, O.Direction, 0.1);
  AssertVectorEquals(Vector3(Sqrt(2), 0, Sqrt(2)).Normalize, O.Up, 0.1);

  { UpPrefer corrects up vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.UpPrefer(Vector3(10, 0, 10));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(Vector3(0, 0, 1), O.Up, 0.1);

  FreeAndNil(O);
end;

procedure TTestCastleTransform.TestViewVectorsOrthogonal2;
{ Test forcing Direction/Up orthogonal by various TCastleTransform routines
  (that actually implement it by TWalkCamera routines).
  This tests does pass dir/up parallel. }
var
  O: TCastleTransform;
begin
  O := TCastleTransform.Create(nil);

  { SetView corrects up vector angle }
  O.SetView(Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 0, 0));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(AnyOrthogonalVector(Vector3(1, 0, 0)), O.Up, 0.1);

  { SetView with AdjustUp = false corrects direction vector angle }
  O.SetView(Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 0, 0), false);
  AssertVectorEquals(AnyOrthogonalVector(Vector3(1, 0, 0)), O.Direction, 0.1);
  AssertVectorEquals(Vector3(1, 0, 0), O.Up, 0.1);

  { Setting direction corrects up vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.Direction := Vector3(0, 0, 10);
  AssertVectorEquals(Vector3(0, 0, 1), O.Direction, 0.1);
  AssertVectorEquals(AnyOrthogonalVector(Vector3(0, 0, 1)), O.Up, 0.1);

  { Setting up corrects direction vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.Up := Vector3(10, 0, 0);
  AssertVectorEquals(AnyOrthogonalVector(Vector3(1, 0, 0)), O.Direction, 0.1);
  AssertVectorEquals(Vector3(1, 0, 0), O.Up, 0.1);

  { UpPrefer corrects up vector }
  O.SetView(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 0, 1));
  O.UpPrefer(Vector3(10, 0, 0));
  AssertVectorEquals(Vector3(1, 0, 0), O.Direction, 0.1);
  AssertVectorEquals(AnyOrthogonalVector(Vector3(1, 0, 0)), O.Up, 0.1);

  FreeAndNil(O);
end;

procedure TTestCastleTransform.TestListNotification;
var
  O1List: TCastleTransform;
  O1: TCastleTransform;
begin
  {$warnings off} { don't warn about creating with abstract methods here }
  O1 := TCastleTransform.Create(nil); O1.Name := 'O1';
  {$warnings on}
  O1List := TCastleTransform.Create(nil); O1List.Name := 'O1List';

  AssertTrue(O1List.Count = 0);
  O1List.Add(O1);
  AssertTrue(O1List.Count = 1);
  FreeAndNil(O1); // freeing O1 should also remove it from O1List automatically
  AssertTrue(O1List.Count = 0);
  FreeAndNil(O1List);
end;

procedure TTestCastleTransform.TestWorldFull;
begin
  DoTestWorld(false);
end;

procedure TTestCastleTransform.TestWorldPrematureFree;
begin
  DoTestWorld(true);
end;

procedure TTestCastleTransform.DoTestWorld(const PrematureFree: boolean);
var
  World1, World2: TCastleRootTransform;
  O1List, O2List: TCastleTransform;
  O1, O2: TCastleTransform;
begin
  World1 := nil;
  World2 := nil;
  try
    {$warnings off} { don't warn about creating with abstract methods here }
    World1 := TCastleRootTransform.Create(nil); World1.Name := 'World1';
    World2 := TCastleRootTransform.Create(nil); World2.Name := 'World2';
    O1 := TCastleTransform.Create(World1); O1.Name := 'O1';
    O2 := TCastleTransform.Create(World1); O2.Name := 'O2';
    {$warnings on}

    O1List := TCastleTransform.Create(World1); O1List.Name := 'O1List';
    O2List := TCastleTransform.Create(World1); O2List.Name := 'O2List';

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

    AssertTrue(nil = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    World2.Add(O1);
    AssertEquals(1, World2.Count);

    AssertTrue(World2 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    { these are incorrect (you should remove object from previous world first) }

    // TODO: The exceptions are raised correctly by these,
    // but when the ECannotAddToAnotherWorld is fired,
    // it breaks in the middle of the addition, and the state is messed up,
    // making later problems when freeing.

    // try
    //   World1.Add(O1);
    //   raise Exception.Create('Adding T3D to different World should not be possible');
    // except on E: ECannotAddToAnotherWorld do ; end;

    // try
    //   World2.Add(O1List);
    //   raise Exception.Create('Adding T3D to different World should not be possible');
    // except on E: ECannotAddToAnotherWorld do ; end;
  finally
    FreeAndNil(World1);
    FreeAndNil(World2);
  end;
end;

procedure TTestCastleTransform.TestWorldFreeBeforeItem;
var
  World1: TCastleRootTransform;
  O1List: TCastleTransform;
  O1: TCastleTransform;
begin
  {$warnings off} { don't warn about creating with abstract methods here }
  World1 := TCastleRootTransform.Create(nil); World1.Name := 'World1';
  O1 := TCastleTransform.Create(nil); O1.Name := 'O1';
  {$warnings on}
  O1List := TCastleTransform.Create(nil); O1List.Name := 'O1List';

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

procedure TTestCastleTransform.TestDirectionUp_UpYDirectionMinusZ;
var
  T: TCastleTransform;
begin
  T := TCastleTransform.Create(nil);
  try
    T.Orientation := otUpYDirectionMinusZ;

    AssertVectorEquals(Vector3(1, 2, 3),
      RotatePointAroundAxis(Vector4(0, 0, 0, 0), Vector3(1, 2, 3)));

    AssertVectorEquals(Vector4(0, 0, 0, 0), T.Rotation);
    AssertVectorEquals(Vector3(0, 0, -1), T.Direction);
    AssertVectorEquals(Vector3(0, 1, 0), T.Up);

    T.Orientation := otUpZDirectionX;
    AssertVectorEquals(Vector3(1, 0, 0), T.Direction);
    AssertVectorEquals(Vector3(0, 0, 1), T.Up);

    T.Direction := Vector3(1, 0, 0);
    AssertVectorEquals(Vector3(1, 0, 0), T.Direction);
    AssertVectorEquals(Vector3(0, 0, 1), T.Up);

    T.Up := Vector3(0, 0, 1);
    AssertVectorEquals(Vector3(1, 0, 0), T.Direction);
    AssertVectorEquals(Vector3(0, 0, 1), T.Up);

    T.Direction := Vector3(1, 1, 1);
    AssertVectorEquals(T.Direction, Vector3(1, 1, 1).Normalize);
  finally FreeAndNil(T) end;
end;

procedure TTestCastleTransform.TestDirectionUp_UpYDirectionZ;
var
  T: TCastleTransform;
begin
  T := TCastleTransform.Create(nil);
  try
    //T.Orientation := otUpYDirectionZ; // should be default
    Assert(TCastleTransform.DefaultOrientation = otUpYDirectionZ);
    Assert(T.Orientation = otUpYDirectionZ);

    AssertVectorEquals(Vector3(1, 2, 3),
      RotatePointAroundAxis(Vector4(0, 0, 0, 0), Vector3(1, 2, 3)));

    AssertVectorEquals(Vector4(0, 0, 0, 0), T.Rotation);
    AssertVectorEquals(Vector3(0, 0, 1), T.Direction);
    AssertVectorEquals(Vector3(0, 1, 0), T.Up);

    T.Direction := Vector3(1, 0, 0);
    AssertVectorEquals(Vector3(1, 0, 0), T.Direction);
    AssertVectorEquals(Vector3(0, 1, 0), T.Up, 0.01);

    T.Up := Vector3(0, 0, 1);
    AssertVectorEquals(Vector3(1, 0, 0), T.Direction);
    AssertVectorEquals(Vector3(0, 0, 1), T.Up);

    T.Direction := Vector3(1, 1, 1);
    AssertVectorEquals(Vector3(1, 1, 1).Normalize, T.Direction);
  finally FreeAndNil(T) end;
end;

procedure TTestCastleTransform.TestTransformingScene;
var
  World: TCastleRootTransform;

  function EpsilonBox(const Center: TVector3): TBox3D;
  begin
    Result := Box3D(
      Center - Vector3(0.01, 0.01, 0.01),
      Center + Vector3(0.01, 0.01, 0.01)
    );
  end;

  procedure AssertBox2(const TestName: string; const T: TCastleTransform; const B: TBox3D);
  var
    P: TVector3;
  begin
    //Writeln(TestName);
    AssertBoxesEqual(B, T.BoundingBox);

    { This has a right to not work in case of using octrees for Scene,
      as then the scene collides as a set of triangles,
      and the TBoxNode is considered empty inside. }
    // AssertTrue(TestName + '_Center', World.WorldBoxCollision(EpsilonBox(B.Center)));

    AssertTrue(TestName + '_Center', World.WorldBoxCollision(B));
    AssertTrue(TestName + '_SphereCenter', World.WorldSphereCollision(B.Center, B.MaxSize));

    P := B.Center;
    P.X := B.Min.X;
    AssertTrue(TestName + '_Min.X', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Y := B.Min.Y;
    AssertTrue(TestName + '_Min.Y', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Z := B.Min.Z;
    AssertTrue(TestName + '_Min.Z', World.WorldBoxCollision(EpsilonBox(P)));

    P := B.Center;
    P.X := B.Max.X;
    AssertTrue(TestName + '_Max.X', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Y := B.Max.Y;
    AssertTrue(TestName + '_Max.Y', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Z := B.Max.Z;
    AssertTrue(TestName + '_Max.Z', World.WorldBoxCollision(EpsilonBox(P)));

    P := B.Center;
    P.X := B.Min.X - 0.1;
    AssertFalse(TestName + '_Outside_Min.X', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Y := B.Min.Y - 0.1;
    AssertFalse(TestName + '_Outside_Min.Y', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Z := B.Min.Z - 0.1;
    AssertFalse(TestName + '_Outside_Min.Z', World.WorldBoxCollision(EpsilonBox(P)));

    P := B.Center;
    P.X := B.Max.X + 0.1;
    AssertFalse(TestName + '_Outside_Max.X', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Y := B.Max.Y + 0.1;
    AssertFalse(TestName + '_Outside_Max.Y', World.WorldBoxCollision(EpsilonBox(P)));
    P := B.Center;
    P.Z := B.Max.Z + 0.1;
    AssertFalse(TestName + '_Outside_Max.Z', World.WorldBoxCollision(EpsilonBox(P)));
  end;

var
  Scene: TCastleSceneCore;

  procedure AssertBox(const TestName: string; const T: TCastleTransform; const B: TBox3D);
  begin
    Scene.Spatial := [];
    AssertBox2(TestName + '_Scene as bbox', T, B);
    Scene.Spatial := [ssDynamicCollisions];
    AssertBox2(TestName + '_Scene as ssDynamicCollisions octree', T, B);
    Scene.Spatial := [ssStaticCollisions];
    AssertBox2(TestName + '_Scene as ssStaticCollisions octree', T, B);
  end;

var
  T: TCastleTransform;
  Box: TBoxNode;
  Shape: TShapeNode;
  Root: TX3DRootNode;
begin
  World := TCastleRootTransform.Create(nil);
  try
    //Box := TBoxNode.CreateWithShape(Shape);
    Box := TBoxNode.Create;
    Shape := TShapeNode.Create;
    Shape.Geometry := Box;
    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);
    Scene := TCastleSceneCore.Create(World);
    Scene.Load(Root, true);

    T := TCastleTransform.Create(World);
    T.Add(Scene);

    World.Add(T);

    AssertBox('Base', T, Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)));

    T.Translation := Vector3(10, 20, 30);
    T.Scale := Vector3(1, 1, 1); // default
    Scene.Translation := Vector3(0, 0, 0); // default
    Scene.Scale := Vector3(1, 1, 1); // default
    AssertBox('T.Translation', T, Box3D(Vector3(10-1, 20-1, 30-1), Vector3(10+1, 20+1, 30+1)));

    T.Translation := Vector3(10, 20, 30);
    T.Scale := Vector3(1, 1, 1); // default
    Scene.Translation := Vector3(100, 200, 300);
    Scene.Scale := Vector3(1, 1, 1); // default
    AssertBox('T.Translation + Scene.Translation', T, Box3D(Vector3(110-1, 220-1, 330-1), Vector3(110+1, 220+1, 330+1)));

    T.Translation := Vector3(0, 0, 0); // default
    T.Scale := Vector3(1, 1, 1); // default
    Scene.Translation := Vector3(100, 200, 300);
    Scene.Scale := Vector3(1, 1, 1); // default
    AssertBox('Scene.Translation', T, Box3D(Vector3(100-1, 200-1, 300-1), Vector3(100+1, 200+1, 300+1)));

    T.Translation := Vector3(10, 20, 30);
    T.Scale := Vector3(0.5, 2.0, 4.0);
    Scene.Translation := Vector3(0, 0, 0); // default
    Scene.Scale := Vector3(1, 1, 1); // default
    AssertBox('T.Translation+Scale', T, Box3D(Vector3(10-0.5, 20-2, 30-4), Vector3(10+0.5, 20+2, 30+4)));

    T.Translation := Vector3(10, 20, 30);
    T.Scale := Vector3(0.5, 0.5, 0.5);
    Scene.Translation := Vector3(100, 200, 300); // default
    Scene.Scale := Vector3(1, 1, 1); // default
    AssertBox('T.Translation+Scale + Scene.Translation', T, Box3D(Vector3(50+10-0.5, 100+20-0.5, 150+30-0.5), Vector3(50+10+0.5, 100+20+0.5, 150+30+0.5)));

    T.Translation := Vector3(10, 20, 30);
    T.Scale := Vector3(0.5, 0.5, 0.5);
    Scene.Translation := Vector3(100, 200, 300); // default
    Scene.Scale := Vector3(4, 4, 4);
    AssertBox('T.Translation+Scale + Scene.Translation+Scale', T, Box3D(Vector3(50+10-2, 100+20-2, 150+30-2), Vector3(50+10+2, 100+20+2, 150+30+2)));
  finally FreeAndNil(World) end;
end;

procedure TTestCastleTransform.TestPhysicsWorldOwnerEmptyBox;
var
  Viewport: TCastleViewport;
  Scene: TCastleSceneCore;
  Body: TRigidBody;
  Collider: TBoxCollider;
begin
  try
    Viewport := TCastleViewport.Create(nil);
    try
      Scene := TCastleSceneCore.Create(Viewport.Items);

      Body := TRigidBody.Create(Viewport.Items);

      Collider := TBoxCollider.Create(Body);

      // add to Viewport before setting Scene.RigidBody,
      // to provoke RigidBody.InitializeTransform to create all physics stuff
      Viewport.Items.Add(Scene);

      Scene.RigidBody := Body;
    finally FreeAndNil(Viewport) end;

    Fail('This should raise EPhysicsError, as TBoxCollider is empty');
  except on EPhysicsError do end;
end;

procedure TTestCastleTransform.TestPhysicsWorldOwnerEmptySphere;
var
  Viewport: TCastleViewport;
  Scene: TCastleSceneCore;
  Body: TRigidBody;
  Collider: TSphereCollider;
begin
  //try
    Viewport := TCastleViewport.Create(nil);
    try
      Scene := TCastleSceneCore.Create(Viewport.Items);

      Body := TRigidBody.Create(Viewport.Items);

      Collider := TSphereCollider.Create(Body);

      // add to Viewport before setting Scene.RigidBody,
      // to provoke RigidBody.InitializeTransform to create all physics stuff
      Viewport.Items.Add(Scene);

      Scene.RigidBody := Body;
    finally FreeAndNil(Viewport) end;

    // OK, this can work without error now,
    // although it's a little inconsistent with TestPhysicsWorldOwnerEmptyBox.

    // Fail('This should raise EPhysicsError, as TSphereCollider is empty');
  //except on EPhysicsError do end;
end;

procedure TTestCastleTransform.TestPhysicsExistsAfterParentTransformExistenceChange;
var
  Viewport: TCastleViewport;
  ParentTransform: TCastleTransform;
  TransformWithRigidBody: TCastleTransform;
  Body: TRigidBody;
  Collider: TBoxCollider;
  FakeRemoveMe: TRemoveType;
begin
  Viewport := TCastleViewport.Create(nil);
  try
    ParentTransform := TCastleTransform.Create(Viewport.Items);
    Viewport.Items.Add(ParentTransform);

    TransformWithRigidBody := TCastleTransform.Create(Viewport.Items);

    Body := TRigidBody.Create(Viewport.Items);

    Collider := TBoxCollider.Create(Body);
    Collider.Size := Vector3(2, 2, 2);

    TransformWithRigidBody.RigidBody := Body;

    ParentTransform.Add(TransformWithRigidBody);

    if not ((krbfActive in Body.InternalKraftBody.Flags) or
       (ksfCollision in Body.Collider.InternalKraftShape.Flags) or
       (ksfRayCastable in Body.Collider.InternalKraftShape.Flags)) then
    begin
         Fail('Wrong Kraft flags in rigid body that exist.');
    end;

    ParentTransform.Exists := false;

    Viewport.Items.Update(1, FakeRemoveMe);

    if (krbfActive in Body.InternalKraftBody.Flags) or
       (ksfCollision in Body.Collider.InternalKraftShape.Flags) or
       (ksfRayCastable in Body.Collider.InternalKraftShape.Flags) then
    begin
      Fail('Wrong Kraft flags in rigid body that parent not exist.');
    end;

  finally FreeAndNil(Viewport) end;
end;

procedure TTestCastleTransform.TestPhysicsWorldOwner;
var
  Viewport: TCastleViewport;
  Scene: TCastleSceneCore;
  Body: TRigidBody;
  Collider: TBoxCollider;
begin
  Viewport := TCastleViewport.Create(nil);
  try
    Scene := TCastleSceneCore.Create(Viewport.Items);

    Body := TRigidBody.Create(Viewport.Items);

    Collider := TBoxCollider.Create(Body);
    Collider.Size := Vector3(2, 2, 2);

    // add to Viewport before setting Scene.RigidBody,
    // to provoke RigidBody.InitializeTransform to create all physics stuff
    Viewport.Items.Add(Scene);

    Scene.RigidBody := Body;
  finally FreeAndNil(Viewport) end;
end;

procedure TTestCastleTransform.TestPass;
var
  A: TInternalRenderingPass;
  B: TUserRenderingPass;
  C: TInternalSceneRenderingPass;
  P: TTotalRenderingPass;
begin
  A := High(A);
  B := High(B);
  C := High(C);
  P := High(P);
  AssertTrue(
    (Integer(A) + 1) *
    (Integer(B) + 1) *
    (Integer(C) + 1) =
    (Integer(P) + 1));
end;

procedure TTestCastleTransform.TestPassCombine;

  { Copied from src/x3d/opengl/castlerenderer.pas }
  function GetTotalPass(
    const Digits: array of Cardinal;
    const Ranges: array of Cardinal): Cardinal;
  var
    I: Integer;
    Multiplier: Cardinal;
  begin
    Result := 0;
    Multiplier := 1;
    Assert(Length(Digits) = Length(Ranges));
    for I := 0 to Length(Digits) - 1 do
    begin
      Result := Result + Digits[I] * Multiplier;
      Multiplier := Multiplier * Ranges[I];
    end;
  end;

begin
  { You can look at 1st GetTotalPass argument as a number, written backwards,
    and then the assertions below are ordered:

    000
    001
    010
    011

    100
    101
    ...

    ... until final:
    211
  }

  AssertEquals(0, GetTotalPass([0, 0, 0], [2, 2, 3]));
  AssertEquals(1, GetTotalPass([1, 0, 0], [2, 2, 3]));
  AssertEquals(2, GetTotalPass([0, 1, 0], [2, 2, 3]));
  AssertEquals(3, GetTotalPass([1, 1, 0], [2, 2, 3]));

  AssertEquals(4, GetTotalPass([0, 0, 1], [2, 2, 3]));
  AssertEquals(5, GetTotalPass([1, 0, 1], [2, 2, 3]));
  AssertEquals(6, GetTotalPass([0, 1, 1], [2, 2, 3]));
  AssertEquals(7, GetTotalPass([1, 1, 1], [2, 2, 3]));

  AssertEquals(8, GetTotalPass([0, 0, 2], [2, 2, 3]));
  AssertEquals(9, GetTotalPass([1, 0, 2], [2, 2, 3]));
  AssertEquals(10, GetTotalPass([0, 1, 2], [2, 2, 3]));
  AssertEquals(11, GetTotalPass([1, 1, 2], [2, 2, 3]));
end;

procedure TTestCastleTransform.TestForIn;
var
  Owner: TComponent;
  T, T1, C: TCastleTransform;
  Y: Single;
begin
  Owner := TComponent.Create(nil);
  try
    T := TCastleTransform.Create(Owner);
    T.Translation := Vector3(1, 0, 0);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 1, 0);
    T.Add(T1);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 2, 0);
    T.Add(T1);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 3, 0);
    T.Add(T1);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 1, 1);
    T[0].Add(T1);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 1, 2);
    T[0].Add(T1);

    T1 := TCastleTransform.Create(Owner);
    T1.Translation := Vector3(1, 1, 3);
    T[0].Add(T1);

    Y := 1;
    for C in T do
    begin
      AssertVectorEquals(C.Translation, Vector3(1, Y, 0));
      Y += 1;
    end;
    AssertSameValue(Y, 4);
  finally FreeAndNil(Owner) end;
end;

initialization
  RegisterTest(TTestCastleTransform);
end.
