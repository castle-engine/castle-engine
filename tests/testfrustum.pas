{
  Copyright 2005-2008 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

unit TestFrustum;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  VectorMath, Boxes3d, Frustum;

type
  TTestFrustum = class(TTestCase)
  private
    procedure AssertFrustumSphereCollisionPossible(const Frustum: TFrustum;
      const SphereCenter: TVector3Single; const SphereRadiusSqt: Single;
      const GoodResult: TFrustumCollisionPossible);
    procedure AssertFrustumBox3dCollisionPossible(const Frustum: TFrustum;
      const Box3d: TBox3d; const GoodResult: TFrustumCollisionPossible);
  published
    procedure TestFrustum;
    procedure TestInfiniteFrustum;
  end;

implementation

procedure TTestFrustum.AssertFrustumSphereCollisionPossible(const Frustum: TFrustum;
  const SphereCenter: TVector3Single; const SphereRadiusSqt: Single;
  const GoodResult: TFrustumCollisionPossible);
begin
 Assert( Frustum.SphereCollisionPossible(SphereCenter,
   SphereRadiusSqt) = GoodResult);

 Assert( Frustum.SphereCollisionPossibleSimple(SphereCenter,
     SphereRadiusSqt) = (GoodResult <> fcNoCollision) );
end;

procedure TTestFrustum.AssertFrustumBox3dCollisionPossible(const Frustum: TFrustum;
  const Box3d: TBox3d; const GoodResult: TFrustumCollisionPossible);
begin
 Assert( Frustum.Box3dCollisionPossible(Box3d) = GoodResult);

 Assert( Frustum.Box3dCollisionPossibleSimple(Box3d) =
   (GoodResult <> fcNoCollision) );
end;

procedure TTestFrustum.TestFrustum;
var
  Frustum: TFrustum;
begin
 { Calculate testing frustum }
 Frustum.Init(
   PerspectiveProjMatrixDeg(60, 1, 10, 100),
   LookDirMatrix(
     Vector3Single(10, 10, 10) { eye position },
     Vector3Single(1, 0, 0) { look direction },
     vector3Single(0, 0, 1) { up vector } ));
 Assert(not Frustum.ZFarInfinity);

 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 81,
   fcNoCollision);
 { This is between camera pos and near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 200,
   fcNoCollision);
 { This should collide with frustum, as it crosses near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 420,
   fcSomeCollisionPossible);
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(50, 10, 10), 1,
   fcInsideFrustum);
 { This sphere intersects near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(20, 10, 10), 1,
   fcSomeCollisionPossible);

 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(-1, -1, -1), Vector3Single(9, 9, 9)),
   fcNoCollision);
 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(50, 10, 10), Vector3Single(51, 11, 11)),
   fcInsideFrustum);
 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(19, 10, 10), Vector3Single(21, 11, 11)),
   fcSomeCollisionPossible);
end;

procedure TTestFrustum.TestInfiniteFrustum;
var
  Frustum: TFrustum;
begin
  Frustum.Init(
    PerspectiveProjMatrixDeg(60, 1, 10, ZFarInfinity),
    LookDirMatrix(
      Vector3Single(10, 10, 10) { eye position },
      Vector3Single(1, 0, 0) { look direction },
      vector3Single(0, 0, 1) { up vector } ));

  Assert(Frustum.Planes[fpFar][0] = 0);
  Assert(Frustum.Planes[fpFar][1] = 0);
  Assert(Frustum.Planes[fpFar][2] = 0);
  Assert(Frustum.ZFarInfinity);

  AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 81,
    fcNoCollision);
  AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(100, 10, 10), 1,
    fcInsideFrustum);
  AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 400,
    fcSomeCollisionPossible);
end;

initialization
 RegisterTest(TTestFrustum);
end.
