// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleCurves" -*-
{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleCurves unit. }
unit TestCastleCurves;

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry, CastleTestCase
  {$else}CastleTester{$endif};

type
  TTestCastleCurves = class(TCastleTestCase)
  published
    procedure TestBoundingBox;
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

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleCurves);
{$endif}
end.
