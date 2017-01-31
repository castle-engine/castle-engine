{
  Copyright 2011-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ }
unit TestCastleTriangulate;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleVectors,
  CastleTriangulate, CastleTriangles;

type
  TTestCastleTriangulate = class(TTestCase)
  private
    { private vars for Face callback }
    Vertexes: PVector3Single;
    CountVertexes: Integer;
    procedure Face(const Tri: TVector3Longint);
    procedure OnWarningRaiseException(Sender: TObject; const Category, S: string);
  published
    procedure TestIndexedConcavePolygonNormal;
    procedure TestTriangulateFace;
  end;

implementation

uses CastleStringUtils, CastleUtils, CastleLog, CastleApplicationProperties;

procedure TTestCastleTriangulate.OnWarningRaiseException(Sender: TObject; const Category, S: string);
begin
  raise Exception.CreateFmt('CastleTriangulate made warning: %s: %s', [Category, S]);
end;

procedure TTestCastleTriangulate.TestIndexedConcavePolygonNormal;
const
  Verts: array [0..3] of TVector3Single =
  ( (-1, 0, 0),
    (0, 1, 0),
    (1, -1, 0),
    (0, 0, 0)
  );
  Indexes: array [0..3] of LongInt = (0, 1, 2, 3);
begin
  AssertTrue(VectorsEqual(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single, false),
    Vector3Single(0, 0, -1)));

  { This is an example polygon that cannot be handled
    by IndexedConvexPolygonNormal }
  AssertTrue(not VectorsEqual(
    IndexedConvexPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single),
    Vector3Single(0, 0, -1)));
  AssertTrue(not VectorsEqual(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single, true),
    Vector3Single(0, 0, -1)));
end;

procedure TTestCastleTriangulate.Face(const Tri: TVector3Longint);
var
  V0, V1, V2, EarNormal: TVector3Single;
begin
  V0 := Vertexes[Tri[0]];
  V1 := Vertexes[Tri[1]];
  V2 := Vertexes[Tri[2]];
  EarNormal := TriangleDir(V0, V1, V2);
  AssertTrue(not ZeroVector(EarNormal));
end;

procedure TTestCastleTriangulate.TestTriangulateFace;

  procedure DoPolygon(AVertexes: array of TVector3Single;
    const Name: string; const RevertOrder: boolean);
  var
    I: Integer;
  begin
    Vertexes := @AVertexes;
    CountVertexes := High(AVertexes) + 1;

    if RevertOrder then
      for I := 0 to CountVertexes div 2 - 1 do
        SwapValues(Vertexes[I], Vertexes[CountVertexes - 1 - I]);

     TriangulateFace(nil, CountVertexes, Vertexes, CountVertexes, @Face, 0);
  end;

type
  TVector3SingleArray = array of TVector3Single;

  procedure GenerateCircle(const Count: Cardinal; var Vertexes: TVector3SingleArray);
  var
    Angle: Float;
    I: Integer;
  begin
    SetLength(Vertexes, Count);
    for I := 0 to Count - 1 do
    begin
      Angle := 2 * Pi * I / Count;
      Vertexes[I] := Vector3Single(Cos(Angle), 0, Sin(Angle));
    end;
  end;

const
  { From polygon_3_5.wrl corrected by JA by removing dup vertexes
    in TriangulateFace. }
  Polygon_3_5: array [0..6] of TVector3Single = (
    (0.216, 0, 0.413),
    (0.528, 0, 0.000),
    (1.000, 0, 0.913),
    (0.528, 0, 0.413),
    (0.316, 0, 1.000),
    (0.000, 0, 0.630),
    (0.528, 0, 0.413)
  );

  Polygon_R3D_cs: array [0..25] of TVector3Single = (
    (8.255, 0, 5.929),
    (8.255, 0, 6.024),
    (8.255, 0, 7.524),
    (8.255, 0, 8.400),
    (5.948, 0, 8.400),
    (4.218, 0, 8.400),
    (0.145, 0, 8.400),
    (0.145, 0, 0.000),
    (1.193, 0, 2.649), // 3x

    (1.193, 0, 3.942),
    (2.222, 0, 3.942),
    (2.222, 0, 2.649),

    (1.193, 0, 2.649), // 3x

    (0.866, 0, 1.343),
    (1.544, 0, 1.343),
    (1.544, 0, 0.665),
    (0.866, 0, 0.665),
    (0.866, 0, 1.343),

    (1.193, 0, 2.649), // 3x

    (0.145, 0, 0.000),
    (4.218, 0, 0.000),
    (7.455, 0, 0.000),
    (8.255, 0, 0.000),
    (8.255, 0, 0.800),
    (8.255, 0, 3.516),
    (8.255, 0, 3.669)
  );

  Polygon_R3D_cs_full: array [0..39] of TVector3Single = (
    (7.087, 0, 5.929),
    (8.255, 0, 5.929),
    (8.255, 0, 6.024),
    (8.255, 0, 7.524),
    (8.255, 0, 8.400),
    (5.948, 0, 8.400),
    (4.218, 0, 8.400),
    (0.145, 0, 8.400),
    (0.145, 0, 0.000),

    (1.193, 0, 2.649), // 3x

    (1.193, 0, 3.942), // 1: Block 1 replaced with 2 to fix
    (2.222, 0, 3.942), // 1:
    (2.222, 0, 2.649), // 1:

    (1.193, 0, 2.649), // 3x

    (0.866, 0, 1.343), // 2:
    (1.544, 0, 1.343), // 2:
    (1.544, 0, 0.665), // 2:
    (0.866, 0, 0.665), // 2:
    (0.866, 0, 1.343), // 2:

    (1.193, 0, 2.649), // 3x

    (0.145, 0, 0.000),
    (4.218, 0, 0.000),
    (7.455, 0, 0.000),
    (8.255, 0, 0.000),
    (8.255, 0, 0.800),
    (8.255, 0, 3.516),
    (8.255, 0, 3.669),
    (7.087, 0, 3.669),
    (6.730, 0, 2.436),
    (6.730, 0, 1.557),

    (5.813, 0, 1.557),
    (5.813, 0, 2.436),
    (6.730, 0, 2.436),
    (7.087, 0, 3.669),
    (7.087, 0, 5.929),
    (6.014, 0, 5.461),
    (6.014, 0, 4.495),
    (5.110, 0, 4.495),
    (5.110, 0, 5.461),
    (6.014, 0, 5.461)
  );

  Polygon_RoomArranger_Cave: array [0..223] of TVector3Single = (
    (0.099, 0, 0.166),
    (0.104, 0, 0.183),
    (0.113, 0, 0.202),
    (0.139, 0, 0.224),
    (0.166, 0, 0.253),
    (0.181, 0, 0.278),
    (0.187, 0, 0.304),
    (0.200, 0, 0.311),
    (0.188, 0, 0.333),
    (0.188, 0, 0.373),
    (0.200, 0, 0.403),
    (0.200, 0, 0.425),
    (0.188, 0, 0.460),
    (0.188, 0, 0.477),
    (0.188, 0, 0.494),
    (0.203, 0, 0.520),
    (0.222, 0, 0.538),
    (0.272, 0, 0.538),
    (0.292, 0, 0.538),
    (0.316, 0, 0.538),
    (0.329, 0, 0.543),
    (0.339, 0, 0.543),
    (0.355, 0, 0.543),
    (0.372, 0, 0.543),
    (0.380, 0, 0.551),
    (0.409, 0, 0.532),
    (0.427, 0, 0.532),
    (0.435, 0, 0.555),
    (0.435, 0, 0.566),
    (0.470, 0, 0.566),
    (0.482, 0, 0.555),
    (0.482, 0, 0.544),
    (0.482, 0, 0.531),
    (0.482, 0, 0.521),
    (0.518, 0, 0.495),
    (0.529, 0, 0.474),
    (0.529, 0, 0.457),
    (0.516, 0, 0.426),
    (0.516, 0, 0.406),
    (0.516, 0, 0.390),
    (0.526, 0, 0.371),
    (0.535, 0, 0.356),
    (0.545, 0, 0.336),
    (0.551, 0, 0.325),
    (0.564, 0, 0.325),
    (0.570, 0, 0.325),
    (0.561, 0, 0.340),
    (0.561, 0, 0.356),
    (0.554, 0, 0.379),
    (0.554, 0, 0.397),
    (0.554, 0, 0.417),
    (0.554, 0, 0.431),
    (0.561, 0, 0.456),
    (0.570, 0, 0.469),
    (0.570, 0, 0.485),
    (0.570, 0, 0.502),
    (0.543, 0, 0.538),
    (0.530, 0, 0.551),
    (0.530, 0, 0.557),
    (0.535, 0, 0.567),
    (0.548, 0, 0.575),
    (0.548, 0, 0.589),
    (0.548, 0, 0.601),
    (0.537, 0, 0.601),
    (0.524, 0, 0.615),
    (0.524, 0, 0.623),
    (0.524, 0, 0.623),
    (0.534, 0, 0.646),
    (0.551, 0, 0.655),
    (0.564, 0, 0.655),
    (0.550, 0, 0.693),
    (0.537, 0, 0.706),
    (0.506, 0, 0.733),
    (0.495, 0, 0.745),
    (0.489, 0, 0.755),
    (0.482, 0, 0.768),
    (0.481, 0, 0.779),
    (0.485, 0, 0.787),
    (0.494, 0, 0.793),
    (0.507, 0, 0.798),
    (0.522, 0, 0.802),
    (0.540, 0, 0.806),
    (0.558, 0, 0.809),
    (0.577, 0, 0.813),
    (0.596, 0, 0.817),
    (0.617, 0, 0.824),
    (0.634, 0, 0.840),
    (0.634, 0, 0.862),
    (0.634, 0, 0.888),
    (0.634, 0, 0.913),
    (0.634, 0, 0.925),
    (0.634, 0, 0.942),
    (0.634, 0, 0.968),
    (0.634, 0, 1.000),
    (1.000, 0, 1.000),
    (1.000, 0, 1.000),
    (1.000, 0, 0.716),
    (0.976, 0, 0.710),
    (0.965, 0, 0.695),
    (0.941, 0, 0.695),
    (0.923, 0, 0.695),
    (0.912, 0, 0.707),
    (0.904, 0, 0.719),
    (0.904, 0, 0.735),
    (0.904, 0, 0.753),
    (0.904, 0, 0.773),
    (0.896, 0, 0.779),
    (0.896, 0, 0.798),
    (0.914, 0, 0.821),
    (0.875, 0, 0.850),
    (0.848, 0, 0.871),
    (0.810, 0, 0.899),
    (0.783, 0, 0.917),
    (0.743, 0, 0.900),
    (0.732, 0, 0.883),
    (0.762, 0, 0.848),
    (0.784, 0, 0.842),
    (0.812, 0, 0.859),
    (0.848, 0, 0.778),
    (0.848, 0, 0.778),
    (0.834, 0, 0.767),
    (0.823, 0, 0.750),
    (0.835, 0, 0.721),
    (0.835, 0, 0.721),
    (0.804, 0, 0.699),
    (0.775, 0, 0.690),
    (0.767, 0, 0.690),
    (0.767, 0, 0.678),
    (0.821, 0, 0.618),
    (0.813, 0, 0.601),
    (0.813, 0, 0.584),
    (0.786, 0, 0.561),
    (0.770, 0, 0.538),
    (0.754, 0, 0.531),
    (0.736, 0, 0.525),
    (0.711, 0, 0.525),
    (0.711, 0, 0.514),
    (0.711, 0, 0.514),
    (0.725, 0, 0.488),
    (0.760, 0, 0.443),
    (0.786, 0, 0.443),
    (0.786, 0, 0.443),
    (0.816, 0, 0.434),
    (0.859, 0, 0.422),
    (0.903, 0, 0.394),
    (0.933, 0, 0.330),
    (0.950, 0, 0.281),
    (0.950, 0, 0.281),
    (0.998, 0, 0.255),
    (0.998, 0, 0.255),
    (0.998, 0, 0.000),
    (0.998, 0, 0.000),
    (0.981, 0, 0.006),
    (0.958, 0, 0.040),
    (0.954, 0, 0.058),
    (0.954, 0, 0.077),
    (0.968, 0, 0.089),
    (0.968, 0, 0.113),
    (0.968, 0, 0.137),
    (0.950, 0, 0.143),
    (0.851, 0, 0.190),
    (0.829, 0, 0.255),
    (0.759, 0, 0.264),
    (0.759, 0, 0.247),
    (0.749, 0, 0.238),
    (0.688, 0, 0.202),
    (0.681, 0, 0.192),
    (0.693, 0, 0.173),
    (0.679, 0, 0.160),
    (0.666, 0, 0.143),
    (0.644, 0, 0.109),
    (0.637, 0, 0.097),
    (0.591, 0, 0.089),
    (0.553, 0, 0.081),
    (0.553, 0, 0.206),
    (0.347, 0, 0.206),
    (0.347, 0, 0.184),
    (0.517, 0, 0.184),
    (0.489, 0, 0.064),
    (0.427, 0, 0.041),
    (0.411, 0, 0.035),
    (0.403, 0, 0.048),
    (0.403, 0, 0.077),
    (0.411, 0, 0.098),
    (0.401, 0, 0.109),
    (0.371, 0, 0.117),
    (0.334, 0, 0.109),
    (0.316, 0, 0.101),
    (0.292, 0, 0.089),
    (0.283, 0, 0.072),
    (0.260, 0, 0.057),
    (0.238, 0, 0.057),
    (0.203, 0, 0.061),
    (0.204, 0, 0.067),
    (0.239, 0, 0.062),
    (0.296, 0, 0.137),
    (0.225, 0, 0.182),
    (0.159, 0, 0.101),
    (0.165, 0, 0.078),
    (0.182, 0, 0.071),
    (0.180, 0, 0.065),
    (0.155, 0, 0.071),
    (0.145, 0, 0.071),
    (0.145, 0, 0.097),
    (0.145, 0, 0.106),
    (0.083, 0, 0.098),
    (0.062, 0, 0.089),
    (0.058, 0, 0.074),
    (0.032, 0, 0.066),
    (0.013, 0, 0.072),
    (0.000, 0, 0.089),
    (0.021, 0, 0.110),
    (0.021, 0, 0.121),
    (0.030, 0, 0.130),
    (0.039, 0, 0.136),
    (0.047, 0, 0.140),
    (0.056, 0, 0.142),
    (0.064, 0, 0.143),
    (0.072, 0, 0.144),
    (0.079, 0, 0.145),
    (0.085, 0, 0.147),
    (0.091, 0, 0.151),
    (0.095, 0, 0.157),
    (0.099, 0, 0.166)
  );

  { from https://sourceforge.net/p/castle-engine/tickets/13/ }
  Polygon_Bug13: array [1..18] of TVector3Single = (
    (1, 0, -2.44921e-016),
    (0.932472, 0, -0.361242),
    (0.739009, 0, -0.673696),
    (0.445738, 0, -0.895164),
    (0.00838485, 0, 0.0251547),
    (-0.273664, 0, -0.961825),
    (-0.602635, 0, -0.798017),
    (-0.850218, 0, -0.526432),
    (-0.982973, 0, -0.183749),
    (-0.982973, 0, 0.18375),
    (-0.850217, 0, 0.526433),
    (-0.602634, 0, 0.798018),

    // (-0.850218, 0, -0.526432),
    // (-0.982973, 0, -0.183749),
    // (-0.982973, 0, 0.18375),
    // (-0.850217, 0, 0.526433),
    // (-0.602634, 0, 0.798018),

    (-0.273663, 0, 0.961826),
    (0.0922688, 0, 0.995734),
    (0.445739, 0, 0.895163),
    (0.739009, 0, 0.673695),
    (0.932472, 0, 0.361241),
    (1, 0, -2.44921e-016)
  );
var
  RevertOrder: boolean;
  Polygon_Circle: TVector3SingleArray;
begin
  { Warnings from CastleTriangulate mean that polygon cannot be triangulated.
    They mean errors for tests below, that *must* pass. }
  ApplicationProperties.OnWarning.Add(@OnWarningRaiseException);
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
    ApplicationProperties.OnWarning.Remove(@OnWarningRaiseException);
  end;
end;

initialization
  RegisterTest(TTestCastleTriangulate);
end.
