// -*- compile-command: "./test_single_testcase.sh TTestX3DNodesNurbs" -*-
{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test NURBS from X3DNodes unit. }
unit TestX3DNodesNurbs;

interface

uses
  Classes, SysUtils,
  CastleTester, CastleVectors, X3DNodes;

type
  TTestX3DNodesNurbs = class(TCastleTestCase)
    procedure TestNurbsCurvePoint;
    procedure TestNurbsSurfacePoint;
  end;

implementation

procedure TTestX3DNodesNurbs.TestNurbsCurvePoint;
var
  CurveNode: TNurbsCurveNode;
  Coordinate: TCoordinateNode;
begin
  Coordinate := TCoordinateNode.Create;
  Coordinate.SetPoint([
    Vector3(2.285389, 1.235778, 1.636090),
    Vector3(1, 0, 0),
    Vector3(1.141864, 1.003204, -1.775073),
    Vector3(1, 0, 0),
    Vector3(3.120634, 1.865495, 2.322197)
  ]);

  CurveNode := TNurbsCurveNode.Create;
  CurveNode.ControlPoint := Coordinate;

  AssertVectorEquals(Vector3(2.285389, 1.235778, 1.636090), CurveNode.Point(0));
  AssertVectorEquals(Vector3(3.120634, 1.865495, 2.322197), CurveNode.Point(1));

  FreeAndNil(CurveNode);
end;

procedure TTestX3DNodesNurbs.TestNurbsSurfacePoint;
var
  SurfaceNode: TNurbsPatchSurfaceNode;
  Coordinate: TCoordinateNode;
  OutputNormal: TVector3;
begin
  Coordinate := TCoordinateNode.Create;
  Coordinate.SetPoint([
    Vector3(2, 2, 10),
    Vector3(3, 2, 10),
    Vector3(4, 2, 10),

    Vector3(2, 3, 10),
    Vector3(3, 3, 10),
    Vector3(4, 3, 10),

    Vector3(2, 4, 10),
    Vector3(3, 4, 10),
    Vector3(4, 4, 10)
  ]);

  SurfaceNode := TNurbsPatchSurfaceNode.Create;
  SurfaceNode.ControlPoint := Coordinate;
  SurfaceNode.UDimension := 3;
  SurfaceNode.VDimension := 3;

  // test TNurbsSurfaceNode.Point
  AssertVectorEquals(Vector3(2, 2, 10), SurfaceNode.Point(0, 0));
  AssertVectorEquals(Vector3(4, 4, 10), SurfaceNode.Point(1, 1));

  // test TNurbsSurfaceNode.Point with additional OutputNormal param
  AssertVectorEquals(Vector3(2, 2, 10), SurfaceNode.Point(0, 0, @OutputNormal));
  AssertVectorEquals(Vector3(0, 0, 1), OutputNormal);
  AssertVectorEquals(Vector3(4, 4, 10), SurfaceNode.Point(1, 1, @OutputNormal));
  AssertVectorEquals(Vector3(0, 0, 1), OutputNormal);

  FreeAndNil(SurfaceNode);
end;

initialization
  RegisterTest(TTestX3DNodesNurbs);
end.
