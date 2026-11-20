// -*- compile-command: "./test_single_testcase.sh TTestCastleCurves" -*-
{
  Copyright 2016-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleCurves unit. }
unit TestCastleCurves;

interface

uses CastleTester;

type
  TTestCastleCurves = class(TCastleTestCase)
  published
    procedure TestBoundingBox;
    procedure TestConvexHullIndexes;
  end;

implementation

uses SysUtils, Classes, Math,
  CastleVectors, CastleUtils, CastleCurves, CastleBoxes;

procedure TTestCastleCurves.TestBoundingBox;
var
  Curve: TPiecewiseCubicBezier;
begin
  Curve := TPiecewiseCubicBezier.Create;
  try
    AssertBoxesEqual(TBox3D.Empty, Curve.BoundingBox);
    Curve.UpdateControlPoints;
    AssertBoxesEqual(TBox3D.Empty, Curve.BoundingBox); // still empty

    Curve.ControlPoints.Add(Vector3(3, 4, 5));
    Curve.ControlPoints.Add(Vector3(33, 44, 54));
    Curve.UpdateControlPoints;
    AssertBoxesEqual(Box3D(
      Vector3(3, 4, 5),
      Vector3(33, 44, 54)), Curve.BoundingBox);
  finally FreeAndNil(Curve) end;
end;

procedure TTestCastleCurves.TestConvexHullIndexes;

  { Calculate ConvexHullIndexes of given 2D points,
    return the resulting indexes as a string, separated by spaces. }
  function HullStr(const Points: array of TVector2): String;
  var
    List: TVector3List;
    Hull: TIntegerList;
    I: Integer;
  begin
    List := TVector3List.Create;
    try
      for I := 0 to High(Points) do
        List.Add(Vector3(Points[I], 0));
      Hull := ConvexHullIndexes(List);
      try
        Result := '';
        for I := 0 to Hull.Count - 1 do
          Result := Result + IntToStr(Hull[I]) + ' ';
        Result := Trim(Result);
      finally FreeAndNil(Hull) end;
    finally FreeAndNil(List) end;
  end;

begin
  AssertEquals('0', HullStr([Vector2(5, 5)]));

  AssertEquals('0 1 2 3', HullStr([
    Vector2(0, 0),
    Vector2(2, 0),
    Vector2(2, 1),
    Vector2(0, 1)
  ]));

  { Interior point (1, 0.5) is lower than the last point on the left side
    of the hull (0, 1).
    It was incorrectly included in the hull, making it non-convex. }
  AssertEquals('0 1 2 3', HullStr([
    Vector2(1, 0),
    Vector2(2, 1),
    Vector2(1, 3),
    Vector2(0, 1),
    Vector2(1, 0.5)
  ]));
end;

initialization
  RegisterTest(TTestCastleCurves);
end.
