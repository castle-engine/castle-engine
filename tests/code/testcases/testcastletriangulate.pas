// -*- compile-command: "./test_single_testcase.sh TTestCastleTriangulate" -*-
{
  Copyright 2011-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleTriangulate unit. }
unit TestCastleTriangulate;

interface

uses
  Classes, SysUtils,
  CastleTester, CastleVectors, CastleTriangulate, CastleTriangles;

type
  TTestCastleTriangulate = class(TCastleTestCase)
  private
    { private vars for Face callback }
    Vertexes: PVector3Array;
    CountVertexes: Integer;
    procedure Face(const Tri: TVector3Integer);
  published
    procedure TestIndexedConcavePolygonNormal;
    procedure TestTriangulateFace;
  end;

implementation

uses CastleStringUtils, CastleUtils, CastleLog, CastleApplicationProperties;

procedure TTestCastleTriangulate.TestIndexedConcavePolygonNormal;
const
  Verts: array [0..3] of TVector3 =
  ( (X: -1; Y:  0; Z: 0),
    (X: 0; Y:  1; Z: 0),
    (X: 1; Y: -1; Z: 0),
    (X: 0; Y:  0; Z: 0)
  );
  Indexes: array [0..3] of Int32 = (0, 1, 2, 3);
begin
  AssertVectorEquals(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, TVector3.Zero, false),
    Vector3(0, 0, -1));

  { This is an example polygon that cannot be handled
    by IndexedConvexPolygonNormal }
  AssertTrue(not TVector3.Equals(
    IndexedConvexPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, TVector3.Zero),
    Vector3(0, 0, -1)));
  AssertTrue(not TVector3.Equals(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, TVector3.Zero, true),
    Vector3(0, 0, -1)));
end;

procedure TTestCastleTriangulate.Face(const Tri: TVector3Integer);
var
  V0, V1, V2, EarNormal: TVector3;
begin
  V0 := Vertexes^[Tri[0]];
  V1 := Vertexes^[Tri[1]];
  V2 := Vertexes^[Tri[2]];
  EarNormal := TriangleDirection(V0, V1, V2);
  AssertTrue(not EarNormal.IsZero);
end;

procedure TTestCastleTriangulate.TestTriangulateFace;

  procedure DoPolygon(AVertexes: array of TVector3;
    const Name: string; const RevertOrder: boolean);
  var
    I: Integer;
  begin
    Vertexes := @AVertexes;
    CountVertexes := High(AVertexes) + 1;
    if RevertOrder then
      for I := 0 to CountVertexes div 2 - 1 do
        SwapValues(Vertexes^[I], Vertexes^[CountVertexes - 1 - I]);

    TriangulateFace(nil, CountVertexes, PVector3Array(Vertexes), CountVertexes, {$ifdef FPC}@{$endif}Face, 0);
  end;

type
  TVector3Array = array of TVector3;

  procedure GenerateCircle(const Count: Cardinal; var Vertexes: TVector3Array);
  var
    Angle: Float;
    I: Integer;
  begin
    SetLength(Vertexes, Count);
    for I := 0 to Count - 1 do
    begin
      Angle := 2 * Pi * I / Count;
      Vertexes[I] := Vector3(Cos(Angle), 0, Sin(Angle));
    end;
  end;

const
  { From polygon_3_5.wrl corrected by JA by removing dup vertexes
    in TriangulateFace. }
  Polygon_3_5: array [0..6] of TVector3 = (
    (X: 0.216; Y: 0; Z: 0.413),
    (X: 0.528; Y: 0; Z: 0.000),
    (X: 1.000; Y: 0; Z: 0.913),
    (X: 0.528; Y: 0; Z: 0.413),
    (X: 0.316; Y: 0; Z: 1.000),
    (X: 0.000; Y: 0; Z: 0.630),
    (X: 0.528; Y: 0; Z: 0.413)
  );

  Polygon_R3D_cs: array [0..25] of TVector3 = (
    (X: 8.255; Y: 0; Z: 5.929),
    (X: 8.255; Y: 0; Z: 6.024),
    (X: 8.255; Y: 0; Z: 7.524),
    (X: 8.255; Y: 0; Z: 8.400),
    (X: 5.948; Y: 0; Z: 8.400),
    (X: 4.218; Y: 0; Z: 8.400),
    (X: 0.145; Y: 0; Z: 8.400),
    (X: 0.145; Y: 0; Z: 0.000),
    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 1.193; Y: 0; Z: 3.942),
    (X: 2.222; Y: 0; Z: 3.942),
    (X: 2.222; Y: 0; Z: 2.649),

    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 0.866; Y: 0; Z: 1.343),
    (X: 1.544; Y: 0; Z: 1.343),
    (X: 1.544; Y: 0; Z: 0.665),
    (X: 0.866; Y: 0; Z: 0.665),
    (X: 0.866; Y: 0; Z: 1.343),

    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 0.145; Y: 0; Z: 0.000),
    (X: 4.218; Y: 0; Z: 0.000),
    (X: 7.455; Y: 0; Z: 0.000),
    (X: 8.255; Y: 0; Z: 0.000),
    (X: 8.255; Y: 0; Z: 0.800),
    (X: 8.255; Y: 0; Z: 3.516),
    (X: 8.255; Y: 0; Z: 3.669)
  );

  Polygon_R3D_cs_full: array [0..39] of TVector3 = (
    (X: 7.087; Y: 0; Z: 5.929),
    (X: 8.255; Y: 0; Z: 5.929),
    (X: 8.255; Y: 0; Z: 6.024),
    (X: 8.255; Y: 0; Z: 7.524),
    (X: 8.255; Y: 0; Z: 8.400),
    (X: 5.948; Y: 0; Z: 8.400),
    (X: 4.218; Y: 0; Z: 8.400),
    (X: 0.145; Y: 0; Z: 8.400),
    (X: 0.145; Y: 0; Z: 0.000),

    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 1.193; Y: 0; Z: 3.942), // 1: Block 1 replaced with 2 to fix
    (X: 2.222; Y: 0; Z: 3.942), // 1:
    (X: 2.222; Y: 0; Z: 2.649), // 1:

    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 0.866; Y: 0; Z: 1.343), // 2:
    (X: 1.544; Y: 0; Z: 1.343), // 2:
    (X: 1.544; Y: 0; Z: 0.665), // 2:
    (X: 0.866; Y: 0; Z: 0.665), // 2:
    (X: 0.866; Y: 0; Z: 1.343), // 2:

    (X: 1.193; Y: 0; Z: 2.649), // 3x

    (X: 0.145; Y: 0; Z: 0.000),
    (X: 4.218; Y: 0; Z: 0.000),
    (X: 7.455; Y: 0; Z: 0.000),
    (X: 8.255; Y: 0; Z: 0.000),
    (X: 8.255; Y: 0; Z: 0.800),
    (X: 8.255; Y: 0; Z: 3.516),
    (X: 8.255; Y: 0; Z: 3.669),
    (X: 7.087; Y: 0; Z: 3.669),
    (X: 6.730; Y: 0; Z: 2.436),
    (X: 6.730; Y: 0; Z: 1.557),

    (X: 5.813; Y: 0; Z: 1.557),
    (X: 5.813; Y: 0; Z: 2.436),
    (X: 6.730; Y: 0; Z: 2.436),
    (X: 7.087; Y: 0; Z: 3.669),
    (X: 7.087; Y: 0; Z: 5.929),
    (X: 6.014; Y: 0; Z: 5.461),
    (X: 6.014; Y: 0; Z: 4.495),
    (X: 5.110; Y: 0; Z: 4.495),
    (X: 5.110; Y: 0; Z: 5.461),
    (X: 6.014; Y: 0; Z: 5.461)
  );

  Polygon_RoomArranger_Cave: array [0..223] of TVector3 = (
    (X: 0.099; Y: 0; Z: 0.166),
    (X: 0.104; Y: 0; Z: 0.183),
    (X: 0.113; Y: 0; Z: 0.202),
    (X: 0.139; Y: 0; Z: 0.224),
    (X: 0.166; Y: 0; Z: 0.253),
    (X: 0.181; Y: 0; Z: 0.278),
    (X: 0.187; Y: 0; Z: 0.304),
    (X: 0.200; Y: 0; Z: 0.311),
    (X: 0.188; Y: 0; Z: 0.333),
    (X: 0.188; Y: 0; Z: 0.373),
    (X: 0.200; Y: 0; Z: 0.403),
    (X: 0.200; Y: 0; Z: 0.425),
    (X: 0.188; Y: 0; Z: 0.460),
    (X: 0.188; Y: 0; Z: 0.477),
    (X: 0.188; Y: 0; Z: 0.494),
    (X: 0.203; Y: 0; Z: 0.520),
    (X: 0.222; Y: 0; Z: 0.538),
    (X: 0.272; Y: 0; Z: 0.538),
    (X: 0.292; Y: 0; Z: 0.538),
    (X: 0.316; Y: 0; Z: 0.538),
    (X: 0.329; Y: 0; Z: 0.543),
    (X: 0.339; Y: 0; Z: 0.543),
    (X: 0.355; Y: 0; Z: 0.543),
    (X: 0.372; Y: 0; Z: 0.543),
    (X: 0.380; Y: 0; Z: 0.551),
    (X: 0.409; Y: 0; Z: 0.532),
    (X: 0.427; Y: 0; Z: 0.532),
    (X: 0.435; Y: 0; Z: 0.555),
    (X: 0.435; Y: 0; Z: 0.566),
    (X: 0.470; Y: 0; Z: 0.566),
    (X: 0.482; Y: 0; Z: 0.555),
    (X: 0.482; Y: 0; Z: 0.544),
    (X: 0.482; Y: 0; Z: 0.531),
    (X: 0.482; Y: 0; Z: 0.521),
    (X: 0.518; Y: 0; Z: 0.495),
    (X: 0.529; Y: 0; Z: 0.474),
    (X: 0.529; Y: 0; Z: 0.457),
    (X: 0.516; Y: 0; Z: 0.426),
    (X: 0.516; Y: 0; Z: 0.406),
    (X: 0.516; Y: 0; Z: 0.390),
    (X: 0.526; Y: 0; Z: 0.371),
    (X: 0.535; Y: 0; Z: 0.356),
    (X: 0.545; Y: 0; Z: 0.336),
    (X: 0.551; Y: 0; Z: 0.325),
    (X: 0.564; Y: 0; Z: 0.325),
    (X: 0.570; Y: 0; Z: 0.325),
    (X: 0.561; Y: 0; Z: 0.340),
    (X: 0.561; Y: 0; Z: 0.356),
    (X: 0.554; Y: 0; Z: 0.379),
    (X: 0.554; Y: 0; Z: 0.397),
    (X: 0.554; Y: 0; Z: 0.417),
    (X: 0.554; Y: 0; Z: 0.431),
    (X: 0.561; Y: 0; Z: 0.456),
    (X: 0.570; Y: 0; Z: 0.469),
    (X: 0.570; Y: 0; Z: 0.485),
    (X: 0.570; Y: 0; Z: 0.502),
    (X: 0.543; Y: 0; Z: 0.538),
    (X: 0.530; Y: 0; Z: 0.551),
    (X: 0.530; Y: 0; Z: 0.557),
    (X: 0.535; Y: 0; Z: 0.567),
    (X: 0.548; Y: 0; Z: 0.575),
    (X: 0.548; Y: 0; Z: 0.589),
    (X: 0.548; Y: 0; Z: 0.601),
    (X: 0.537; Y: 0; Z: 0.601),
    (X: 0.524; Y: 0; Z: 0.615),
    (X: 0.524; Y: 0; Z: 0.623),
    (X: 0.524; Y: 0; Z: 0.623),
    (X: 0.534; Y: 0; Z: 0.646),
    (X: 0.551; Y: 0; Z: 0.655),
    (X: 0.564; Y: 0; Z: 0.655),
    (X: 0.550; Y: 0; Z: 0.693),
    (X: 0.537; Y: 0; Z: 0.706),
    (X: 0.506; Y: 0; Z: 0.733),
    (X: 0.495; Y: 0; Z: 0.745),
    (X: 0.489; Y: 0; Z: 0.755),
    (X: 0.482; Y: 0; Z: 0.768),
    (X: 0.481; Y: 0; Z: 0.779),
    (X: 0.485; Y: 0; Z: 0.787),
    (X: 0.494; Y: 0; Z: 0.793),
    (X: 0.507; Y: 0; Z: 0.798),
    (X: 0.522; Y: 0; Z: 0.802),
    (X: 0.540; Y: 0; Z: 0.806),
    (X: 0.558; Y: 0; Z: 0.809),
    (X: 0.577; Y: 0; Z: 0.813),
    (X: 0.596; Y: 0; Z: 0.817),
    (X: 0.617; Y: 0; Z: 0.824),
    (X: 0.634; Y: 0; Z: 0.840),
    (X: 0.634; Y: 0; Z: 0.862),
    (X: 0.634; Y: 0; Z: 0.888),
    (X: 0.634; Y: 0; Z: 0.913),
    (X: 0.634; Y: 0; Z: 0.925),
    (X: 0.634; Y: 0; Z: 0.942),
    (X: 0.634; Y: 0; Z: 0.968),
    (X: 0.634; Y: 0; Z: 1.000),
    (X: 1.000; Y: 0; Z: 1.000),
    (X: 1.000; Y: 0; Z: 1.000),
    (X: 1.000; Y: 0; Z: 0.716),
    (X: 0.976; Y: 0; Z: 0.710),
    (X: 0.965; Y: 0; Z: 0.695),
    (X: 0.941; Y: 0; Z: 0.695),
    (X: 0.923; Y: 0; Z: 0.695),
    (X: 0.912; Y: 0; Z: 0.707),
    (X: 0.904; Y: 0; Z: 0.719),
    (X: 0.904; Y: 0; Z: 0.735),
    (X: 0.904; Y: 0; Z: 0.753),
    (X: 0.904; Y: 0; Z: 0.773),
    (X: 0.896; Y: 0; Z: 0.779),
    (X: 0.896; Y: 0; Z: 0.798),
    (X: 0.914; Y: 0; Z: 0.821),
    (X: 0.875; Y: 0; Z: 0.850),
    (X: 0.848; Y: 0; Z: 0.871),
    (X: 0.810; Y: 0; Z: 0.899),
    (X: 0.783; Y: 0; Z: 0.917),
    (X: 0.743; Y: 0; Z: 0.900),
    (X: 0.732; Y: 0; Z: 0.883),
    (X: 0.762; Y: 0; Z: 0.848),
    (X: 0.784; Y: 0; Z: 0.842),
    (X: 0.812; Y: 0; Z: 0.859),
    (X: 0.848; Y: 0; Z: 0.778),
    (X: 0.848; Y: 0; Z: 0.778),
    (X: 0.834; Y: 0; Z: 0.767),
    (X: 0.823; Y: 0; Z: 0.750),
    (X: 0.835; Y: 0; Z: 0.721),
    (X: 0.835; Y: 0; Z: 0.721),
    (X: 0.804; Y: 0; Z: 0.699),
    (X: 0.775; Y: 0; Z: 0.690),
    (X: 0.767; Y: 0; Z: 0.690),
    (X: 0.767; Y: 0; Z: 0.678),
    (X: 0.821; Y: 0; Z: 0.618),
    (X: 0.813; Y: 0; Z: 0.601),
    (X: 0.813; Y: 0; Z: 0.584),
    (X: 0.786; Y: 0; Z: 0.561),
    (X: 0.770; Y: 0; Z: 0.538),
    (X: 0.754; Y: 0; Z: 0.531),
    (X: 0.736; Y: 0; Z: 0.525),
    (X: 0.711; Y: 0; Z: 0.525),
    (X: 0.711; Y: 0; Z: 0.514),
    (X: 0.711; Y: 0; Z: 0.514),
    (X: 0.725; Y: 0; Z: 0.488),
    (X: 0.760; Y: 0; Z: 0.443),
    (X: 0.786; Y: 0; Z: 0.443),
    (X: 0.786; Y: 0; Z: 0.443),
    (X: 0.816; Y: 0; Z: 0.434),
    (X: 0.859; Y: 0; Z: 0.422),
    (X: 0.903; Y: 0; Z: 0.394),
    (X: 0.933; Y: 0; Z: 0.330),
    (X: 0.950; Y: 0; Z: 0.281),
    (X: 0.950; Y: 0; Z: 0.281),
    (X: 0.998; Y: 0; Z: 0.255),
    (X: 0.998; Y: 0; Z: 0.255),
    (X: 0.998; Y: 0; Z: 0.000),
    (X: 0.998; Y: 0; Z: 0.000),
    (X: 0.981; Y: 0; Z: 0.006),
    (X: 0.958; Y: 0; Z: 0.040),
    (X: 0.954; Y: 0; Z: 0.058),
    (X: 0.954; Y: 0; Z: 0.077),
    (X: 0.968; Y: 0; Z: 0.089),
    (X: 0.968; Y: 0; Z: 0.113),
    (X: 0.968; Y: 0; Z: 0.137),
    (X: 0.950; Y: 0; Z: 0.143),
    (X: 0.851; Y: 0; Z: 0.190),
    (X: 0.829; Y: 0; Z: 0.255),
    (X: 0.759; Y: 0; Z: 0.264),
    (X: 0.759; Y: 0; Z: 0.247),
    (X: 0.749; Y: 0; Z: 0.238),
    (X: 0.688; Y: 0; Z: 0.202),
    (X: 0.681; Y: 0; Z: 0.192),
    (X: 0.693; Y: 0; Z: 0.173),
    (X: 0.679; Y: 0; Z: 0.160),
    (X: 0.666; Y: 0; Z: 0.143),
    (X: 0.644; Y: 0; Z: 0.109),
    (X: 0.637; Y: 0; Z: 0.097),
    (X: 0.591; Y: 0; Z: 0.089),
    (X: 0.553; Y: 0; Z: 0.081),
    (X: 0.553; Y: 0; Z: 0.206),
    (X: 0.347; Y: 0; Z: 0.206),
    (X: 0.347; Y: 0; Z: 0.184),
    (X: 0.517; Y: 0; Z: 0.184),
    (X: 0.489; Y: 0; Z: 0.064),
    (X: 0.427; Y: 0; Z: 0.041),
    (X: 0.411; Y: 0; Z: 0.035),
    (X: 0.403; Y: 0; Z: 0.048),
    (X: 0.403; Y: 0; Z: 0.077),
    (X: 0.411; Y: 0; Z: 0.098),
    (X: 0.401; Y: 0; Z: 0.109),
    (X: 0.371; Y: 0; Z: 0.117),
    (X: 0.334; Y: 0; Z: 0.109),
    (X: 0.316; Y: 0; Z: 0.101),
    (X: 0.292; Y: 0; Z: 0.089),
    (X: 0.283; Y: 0; Z: 0.072),
    (X: 0.260; Y: 0; Z: 0.057),
    (X: 0.238; Y: 0; Z: 0.057),
    (X: 0.203; Y: 0; Z: 0.061),
    (X: 0.204; Y: 0; Z: 0.067),
    (X: 0.239; Y: 0; Z: 0.062),
    (X: 0.296; Y: 0; Z: 0.137),
    (X: 0.225; Y: 0; Z: 0.182),
    (X: 0.159; Y: 0; Z: 0.101),
    (X: 0.165; Y: 0; Z: 0.078),
    (X: 0.182; Y: 0; Z: 0.071),
    (X: 0.180; Y: 0; Z: 0.065),
    (X: 0.155; Y: 0; Z: 0.071),
    (X: 0.145; Y: 0; Z: 0.071),
    (X: 0.145; Y: 0; Z: 0.097),
    (X: 0.145; Y: 0; Z: 0.106),
    (X: 0.083; Y: 0; Z: 0.098),
    (X: 0.062; Y: 0; Z: 0.089),
    (X: 0.058; Y: 0; Z: 0.074),
    (X: 0.032; Y: 0; Z: 0.066),
    (X: 0.013; Y: 0; Z: 0.072),
    (X: 0.000; Y: 0; Z: 0.089),
    (X: 0.021; Y: 0; Z: 0.110),
    (X: 0.021; Y: 0; Z: 0.121),
    (X: 0.030; Y: 0; Z: 0.130),
    (X: 0.039; Y: 0; Z: 0.136),
    (X: 0.047; Y: 0; Z: 0.140),
    (X: 0.056; Y: 0; Z: 0.142),
    (X: 0.064; Y: 0; Z: 0.143),
    (X: 0.072; Y: 0; Z: 0.144),
    (X: 0.079; Y: 0; Z: 0.145),
    (X: 0.085; Y: 0; Z: 0.147),
    (X: 0.091; Y: 0; Z: 0.151),
    (X: 0.095; Y: 0; Z: 0.157),
    (X: 0.099; Y: 0; Z: 0.166)
  );

  { from https://sourceforge.net/p/castle-engine/tickets/13/ }
  Polygon_Bug13: array [1..18] of TVector3 = (
    (X: 1; Y: 0; Z: -2.44921e-016),
    (X: 0.932472; Y: 0; Z: -0.361242),
    (X: 0.739009; Y: 0; Z: -0.673696),
    (X: 0.445738; Y: 0; Z: -0.895164),
    (X: 0.00838485; Y: 0; Z: 0.0251547),
    (X: -0.273664; Y: 0; Z: -0.961825),
    (X: -0.602635; Y: 0; Z: -0.798017),
    (X: -0.850218; Y: 0; Z: -0.526432),
    (X: -0.982973; Y: 0; Z: -0.183749),
    (X: -0.982973; Y: 0; Z: 0.18375),
    (X: -0.850217; Y: 0; Z: 0.526433),
    (X: -0.602634; Y: 0; Z: 0.798018),

    // (X: -0.850218; Y: 0; Z: -0.526432),
    // (X: -0.982973; Y: 0; Z: -0.183749),
    // (X: -0.982973; Y: 0; Z: 0.18375),
    // (X: -0.850217; Y: 0; Z: 0.526433),
    // (X: -0.602634; Y: 0; Z: 0.798018),

    (X: -0.273663; Y: 0; Z: 0.961826),
    (X: 0.0922688; Y: 0; Z: 0.995734),
    (X: 0.445739; Y: 0; Z: 0.895163),
    (X: 0.739009; Y: 0; Z: 0.673695),
    (X: 0.932472; Y: 0; Z: 0.361241),
    (X: 1; Y: 0; Z: -2.44921e-016)
  );
var
  RevertOrder: boolean;
  Polygon_Circle: TVector3Array;
begin
  { Warnings from CastleTriangulate mean that polygon cannot be triangulated.
    They mean errors for tests below, that *must* pass. }
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  try
    GenerateCircle(16, Polygon_Circle);

    for RevertOrder := false to true do
    begin
      DoPolygon(Polygon_Circle, 'Circle', RevertOrder);
      DoPolygon(Polygon_3_5, 'polygon_3_5', RevertOrder);
      DoPolygon(Polygon_R3D_cs, 'R3D_cs', RevertOrder);
      DoPolygon(Polygon_R3D_cs_full, 'R3D_cs_full', RevertOrder);
      DoPolygon(Polygon_RoomArranger_Cave, 'RoomArranger_Cave', RevertOrder);
      DoPolygon(Polygon_Bug13, 'Bug13', RevertOrder);
    end;

    { TODO: test that results are same as hardcoded results? }

  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
  end;
end;

initialization
  RegisterTest(TTestCastleTriangulate);
end.
