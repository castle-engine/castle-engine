// -*- compile-command: "./test_single_testcase.sh TTestCastleTriangles" -*-
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

{ Test CastleTriangles unit. }
unit TestCastleTriangles;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleTriangles = class(TCastleTestCase)
  published
    procedure TestIsValid;
  end;

implementation

uses CastleVectors, CastleTriangles;

procedure TTestCastleTriangles.TestIsValid;
var
  T: TTriangle3;
begin
  { easy test }
  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(0, 10, 0);
  AssertTrue('10', T.IsValid);

  { 3 colinear (or almost colinear) points }

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(5, 0, 0);
  AssertFalse('20', T.IsValid);

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(5, 1, 0);
  AssertTrue('21', T.IsValid);

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(5, 0.000001, 0);
  AssertTrue('22', T.IsValid);

  // same triangle as above, but different order
  T[0] := Vector3(5, 0.000001, 0);
  T[1] := Vector3(0, 0, 0);
  T[2] := Vector3(10, 0, 0);
  AssertTrue('23', T.IsValid);

  // same triangle as above, but different order
  T[0] := Vector3(10, 0, 0);
  T[1] := Vector3(5, 0.000001, 0);
  T[2] := Vector3(0, 0, 0);
  AssertTrue('24', T.IsValid);

  { 3 points not only colinear, but also 2 points over each other }

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 0, 0);
  AssertFalse('30', T.IsValid);

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 1, 0);
  AssertTrue('31', T.IsValid);

  T[0] := Vector3(0, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 0.00000001, 0);
  AssertTrue('32', T.IsValid);

  { 3 points not only colinear, but also 3 points over each other }

  T[0] := Vector3(10, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 0, 0);
  AssertFalse('40', T.IsValid);

  T[0] := Vector3(10, 0, 0);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 0.00000001, 0);
  AssertFalse('41', T.IsValid);

  T[0] := Vector3(10, 0, 0.00000001);
  T[1] := Vector3(10, 0, 0);
  T[2] := Vector3(10, 0.00000001, 0);
  AssertTrue('42', T.IsValid);
end;

initialization
  RegisterTest(TTestCastleTriangles);
end.
