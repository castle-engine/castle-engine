// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleTriangulate" -*-
{
  Copyright 2011-2021 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase, {$else}CastleTester, {$endif} CastleVectors,
  CastleTriangulate, CastleTriangles;

type
  TTestCastleTriangulate = class(TCastleTestCase)
  private
    { private vars for Face callback }
    Vertexes: PVector3;
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
  ( (Data: (-1,  0, 0)),
    (Data: ( 0,  1, 0)),
    (Data: ( 1, -1, 0)),
    (Data: ( 0,  0, 0))
  );
  Indexes: array [0..3] of LongInt = (0, 1, 2, 3);
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
  {$ifndef FPC}{$POINTERMATH ON}{$endif}
  V0 := Vertexes[Tri[0]];
  V1 := Vertexes[Tri[1]];
  V2 := Vertexes[Tri[2]];
  {$ifndef FPC}{$POINTERMATH OFF}{$endif}
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
    {$ifndef FPC}{$POINTERMATH ON}{$endif}
    if RevertOrder then
      for I := 0 to CountVertexes div 2 - 1 do
        SwapValues(Vertexes[I], Vertexes[CountVertexes - 1 - I]);

    {$ifndef FPC}{$POINTERMATH OFF}{$endif}
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
    (Data: (0.216, 0, 0.413)),
    (Data: (0.528, 0, 0.000)),
    (Data: (1.000, 0, 0.913)),
    (Data: (0.528, 0, 0.413)),
    (Data: (0.316, 0, 1.000)),
    (Data: (0.000, 0, 0.630)),
    (Data: (0.528, 0, 0.413))
  );

  Polygon_R3D_cs: array [0..25] of TVector3 = (
    (Data: (8.255, 0, 5.929)),
    (Data: (8.255, 0, 6.024)),
    (Data: (8.255, 0, 7.524)),
    (Data: (8.255, 0, 8.400)),
    (Data: (5.948, 0, 8.400)),
    (Data: (4.218, 0, 8.400)),
    (Data: (0.145, 0, 8.400)),
    (Data: (0.145, 0, 0.000)),
    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (1.193, 0, 3.942)),
    (Data: (2.222, 0, 3.942)),
    (Data: (2.222, 0, 2.649)),

    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (0.866, 0, 1.343)),
    (Data: (1.544, 0, 1.343)),
    (Data: (1.544, 0, 0.665)),
    (Data: (0.866, 0, 0.665)),
    (Data: (0.866, 0, 1.343)),

    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (0.145, 0, 0.000)),
    (Data: (4.218, 0, 0.000)),
    (Data: (7.455, 0, 0.000)),
    (Data: (8.255, 0, 0.000)),
    (Data: (8.255, 0, 0.800)),
    (Data: (8.255, 0, 3.516)),
    (Data: (8.255, 0, 3.669))
  );

  Polygon_R3D_cs_full: array [0..39] of TVector3 = (
    (Data: (7.087, 0, 5.929)),
    (Data: (8.255, 0, 5.929)),
    (Data: (8.255, 0, 6.024)),
    (Data: (8.255, 0, 7.524)),
    (Data: (8.255, 0, 8.400)),
    (Data: (5.948, 0, 8.400)),
    (Data: (4.218, 0, 8.400)),
    (Data: (0.145, 0, 8.400)),
    (Data: (0.145, 0, 0.000)),

    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (1.193, 0, 3.942)), // 1: Block 1 replaced with 2 to fix
    (Data: (2.222, 0, 3.942)), // 1:
    (Data: (2.222, 0, 2.649)), // 1:

    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (0.866, 0, 1.343)), // 2:
    (Data: (1.544, 0, 1.343)), // 2:
    (Data: (1.544, 0, 0.665)), // 2:
    (Data: (0.866, 0, 0.665)), // 2:
    (Data: (0.866, 0, 1.343)), // 2:

    (Data: (1.193, 0, 2.649)), // 3x

    (Data: (0.145, 0, 0.000)),
    (Data: (4.218, 0, 0.000)),
    (Data: (7.455, 0, 0.000)),
    (Data: (8.255, 0, 0.000)),
    (Data: (8.255, 0, 0.800)),
    (Data: (8.255, 0, 3.516)),
    (Data: (8.255, 0, 3.669)),
    (Data: (7.087, 0, 3.669)),
    (Data: (6.730, 0, 2.436)),
    (Data: (6.730, 0, 1.557)),

    (Data: (5.813, 0, 1.557)),
    (Data: (5.813, 0, 2.436)),
    (Data: (6.730, 0, 2.436)),
    (Data: (7.087, 0, 3.669)),
    (Data: (7.087, 0, 5.929)),
    (Data: (6.014, 0, 5.461)),
    (Data: (6.014, 0, 4.495)),
    (Data: (5.110, 0, 4.495)),
    (Data: (5.110, 0, 5.461)),
    (Data: (6.014, 0, 5.461))
  );

  Polygon_RoomArranger_Cave: array [0..223] of TVector3 = (
    (Data: (0.099, 0, 0.166)),
    (Data: (0.104, 0, 0.183)),
    (Data: (0.113, 0, 0.202)),
    (Data: (0.139, 0, 0.224)),
    (Data: (0.166, 0, 0.253)),
    (Data: (0.181, 0, 0.278)),
    (Data: (0.187, 0, 0.304)),
    (Data: (0.200, 0, 0.311)),
    (Data: (0.188, 0, 0.333)),
    (Data: (0.188, 0, 0.373)),
    (Data: (0.200, 0, 0.403)),
    (Data: (0.200, 0, 0.425)),
    (Data: (0.188, 0, 0.460)),
    (Data: (0.188, 0, 0.477)),
    (Data: (0.188, 0, 0.494)),
    (Data: (0.203, 0, 0.520)),
    (Data: (0.222, 0, 0.538)),
    (Data: (0.272, 0, 0.538)),
    (Data: (0.292, 0, 0.538)),
    (Data: (0.316, 0, 0.538)),
    (Data: (0.329, 0, 0.543)),
    (Data: (0.339, 0, 0.543)),
    (Data: (0.355, 0, 0.543)),
    (Data: (0.372, 0, 0.543)),
    (Data: (0.380, 0, 0.551)),
    (Data: (0.409, 0, 0.532)),
    (Data: (0.427, 0, 0.532)),
    (Data: (0.435, 0, 0.555)),
    (Data: (0.435, 0, 0.566)),
    (Data: (0.470, 0, 0.566)),
    (Data: (0.482, 0, 0.555)),
    (Data: (0.482, 0, 0.544)),
    (Data: (0.482, 0, 0.531)),
    (Data: (0.482, 0, 0.521)),
    (Data: (0.518, 0, 0.495)),
    (Data: (0.529, 0, 0.474)),
    (Data: (0.529, 0, 0.457)),
    (Data: (0.516, 0, 0.426)),
    (Data: (0.516, 0, 0.406)),
    (Data: (0.516, 0, 0.390)),
    (Data: (0.526, 0, 0.371)),
    (Data: (0.535, 0, 0.356)),
    (Data: (0.545, 0, 0.336)),
    (Data: (0.551, 0, 0.325)),
    (Data: (0.564, 0, 0.325)),
    (Data: (0.570, 0, 0.325)),
    (Data: (0.561, 0, 0.340)),
    (Data: (0.561, 0, 0.356)),
    (Data: (0.554, 0, 0.379)),
    (Data: (0.554, 0, 0.397)),
    (Data: (0.554, 0, 0.417)),
    (Data: (0.554, 0, 0.431)),
    (Data: (0.561, 0, 0.456)),
    (Data: (0.570, 0, 0.469)),
    (Data: (0.570, 0, 0.485)),
    (Data: (0.570, 0, 0.502)),
    (Data: (0.543, 0, 0.538)),
    (Data: (0.530, 0, 0.551)),
    (Data: (0.530, 0, 0.557)),
    (Data: (0.535, 0, 0.567)),
    (Data: (0.548, 0, 0.575)),
    (Data: (0.548, 0, 0.589)),
    (Data: (0.548, 0, 0.601)),
    (Data: (0.537, 0, 0.601)),
    (Data: (0.524, 0, 0.615)),
    (Data: (0.524, 0, 0.623)),
    (Data: (0.524, 0, 0.623)),
    (Data: (0.534, 0, 0.646)),
    (Data: (0.551, 0, 0.655)),
    (Data: (0.564, 0, 0.655)),
    (Data: (0.550, 0, 0.693)),
    (Data: (0.537, 0, 0.706)),
    (Data: (0.506, 0, 0.733)),
    (Data: (0.495, 0, 0.745)),
    (Data: (0.489, 0, 0.755)),
    (Data: (0.482, 0, 0.768)),
    (Data: (0.481, 0, 0.779)),
    (Data: (0.485, 0, 0.787)),
    (Data: (0.494, 0, 0.793)),
    (Data: (0.507, 0, 0.798)),
    (Data: (0.522, 0, 0.802)),
    (Data: (0.540, 0, 0.806)),
    (Data: (0.558, 0, 0.809)),
    (Data: (0.577, 0, 0.813)),
    (Data: (0.596, 0, 0.817)),
    (Data: (0.617, 0, 0.824)),
    (Data: (0.634, 0, 0.840)),
    (Data: (0.634, 0, 0.862)),
    (Data: (0.634, 0, 0.888)),
    (Data: (0.634, 0, 0.913)),
    (Data: (0.634, 0, 0.925)),
    (Data: (0.634, 0, 0.942)),
    (Data: (0.634, 0, 0.968)),
    (Data: (0.634, 0, 1.000)),
    (Data: (1.000, 0, 1.000)),
    (Data: (1.000, 0, 1.000)),
    (Data: (1.000, 0, 0.716)),
    (Data: (0.976, 0, 0.710)),
    (Data: (0.965, 0, 0.695)),
    (Data: (0.941, 0, 0.695)),
    (Data: (0.923, 0, 0.695)),
    (Data: (0.912, 0, 0.707)),
    (Data: (0.904, 0, 0.719)),
    (Data: (0.904, 0, 0.735)),
    (Data: (0.904, 0, 0.753)),
    (Data: (0.904, 0, 0.773)),
    (Data: (0.896, 0, 0.779)),
    (Data: (0.896, 0, 0.798)),
    (Data: (0.914, 0, 0.821)),
    (Data: (0.875, 0, 0.850)),
    (Data: (0.848, 0, 0.871)),
    (Data: (0.810, 0, 0.899)),
    (Data: (0.783, 0, 0.917)),
    (Data: (0.743, 0, 0.900)),
    (Data: (0.732, 0, 0.883)),
    (Data: (0.762, 0, 0.848)),
    (Data: (0.784, 0, 0.842)),
    (Data: (0.812, 0, 0.859)),
    (Data: (0.848, 0, 0.778)),
    (Data: (0.848, 0, 0.778)),
    (Data: (0.834, 0, 0.767)),
    (Data: (0.823, 0, 0.750)),
    (Data: (0.835, 0, 0.721)),
    (Data: (0.835, 0, 0.721)),
    (Data: (0.804, 0, 0.699)),
    (Data: (0.775, 0, 0.690)),
    (Data: (0.767, 0, 0.690)),
    (Data: (0.767, 0, 0.678)),
    (Data: (0.821, 0, 0.618)),
    (Data: (0.813, 0, 0.601)),
    (Data: (0.813, 0, 0.584)),
    (Data: (0.786, 0, 0.561)),
    (Data: (0.770, 0, 0.538)),
    (Data: (0.754, 0, 0.531)),
    (Data: (0.736, 0, 0.525)),
    (Data: (0.711, 0, 0.525)),
    (Data: (0.711, 0, 0.514)),
    (Data: (0.711, 0, 0.514)),
    (Data: (0.725, 0, 0.488)),
    (Data: (0.760, 0, 0.443)),
    (Data: (0.786, 0, 0.443)),
    (Data: (0.786, 0, 0.443)),
    (Data: (0.816, 0, 0.434)),
    (Data: (0.859, 0, 0.422)),
    (Data: (0.903, 0, 0.394)),
    (Data: (0.933, 0, 0.330)),
    (Data: (0.950, 0, 0.281)),
    (Data: (0.950, 0, 0.281)),
    (Data: (0.998, 0, 0.255)),
    (Data: (0.998, 0, 0.255)),
    (Data: (0.998, 0, 0.000)),
    (Data: (0.998, 0, 0.000)),
    (Data: (0.981, 0, 0.006)),
    (Data: (0.958, 0, 0.040)),
    (Data: (0.954, 0, 0.058)),
    (Data: (0.954, 0, 0.077)),
    (Data: (0.968, 0, 0.089)),
    (Data: (0.968, 0, 0.113)),
    (Data: (0.968, 0, 0.137)),
    (Data: (0.950, 0, 0.143)),
    (Data: (0.851, 0, 0.190)),
    (Data: (0.829, 0, 0.255)),
    (Data: (0.759, 0, 0.264)),
    (Data: (0.759, 0, 0.247)),
    (Data: (0.749, 0, 0.238)),
    (Data: (0.688, 0, 0.202)),
    (Data: (0.681, 0, 0.192)),
    (Data: (0.693, 0, 0.173)),
    (Data: (0.679, 0, 0.160)),
    (Data: (0.666, 0, 0.143)),
    (Data: (0.644, 0, 0.109)),
    (Data: (0.637, 0, 0.097)),
    (Data: (0.591, 0, 0.089)),
    (Data: (0.553, 0, 0.081)),
    (Data: (0.553, 0, 0.206)),
    (Data: (0.347, 0, 0.206)),
    (Data: (0.347, 0, 0.184)),
    (Data: (0.517, 0, 0.184)),
    (Data: (0.489, 0, 0.064)),
    (Data: (0.427, 0, 0.041)),
    (Data: (0.411, 0, 0.035)),
    (Data: (0.403, 0, 0.048)),
    (Data: (0.403, 0, 0.077)),
    (Data: (0.411, 0, 0.098)),
    (Data: (0.401, 0, 0.109)),
    (Data: (0.371, 0, 0.117)),
    (Data: (0.334, 0, 0.109)),
    (Data: (0.316, 0, 0.101)),
    (Data: (0.292, 0, 0.089)),
    (Data: (0.283, 0, 0.072)),
    (Data: (0.260, 0, 0.057)),
    (Data: (0.238, 0, 0.057)),
    (Data: (0.203, 0, 0.061)),
    (Data: (0.204, 0, 0.067)),
    (Data: (0.239, 0, 0.062)),
    (Data: (0.296, 0, 0.137)),
    (Data: (0.225, 0, 0.182)),
    (Data: (0.159, 0, 0.101)),
    (Data: (0.165, 0, 0.078)),
    (Data: (0.182, 0, 0.071)),
    (Data: (0.180, 0, 0.065)),
    (Data: (0.155, 0, 0.071)),
    (Data: (0.145, 0, 0.071)),
    (Data: (0.145, 0, 0.097)),
    (Data: (0.145, 0, 0.106)),
    (Data: (0.083, 0, 0.098)),
    (Data: (0.062, 0, 0.089)),
    (Data: (0.058, 0, 0.074)),
    (Data: (0.032, 0, 0.066)),
    (Data: (0.013, 0, 0.072)),
    (Data: (0.000, 0, 0.089)),
    (Data: (0.021, 0, 0.110)),
    (Data: (0.021, 0, 0.121)),
    (Data: (0.030, 0, 0.130)),
    (Data: (0.039, 0, 0.136)),
    (Data: (0.047, 0, 0.140)),
    (Data: (0.056, 0, 0.142)),
    (Data: (0.064, 0, 0.143)),
    (Data: (0.072, 0, 0.144)),
    (Data: (0.079, 0, 0.145)),
    (Data: (0.085, 0, 0.147)),
    (Data: (0.091, 0, 0.151)),
    (Data: (0.095, 0, 0.157)),
    (Data: (0.099, 0, 0.166))
  );

  { from https://sourceforge.net/p/castle-engine/tickets/13/ }
  Polygon_Bug13: array [1..18] of TVector3 = (
    (Data: (1, 0, -2.44921e-016)),
    (Data: (0.932472, 0, -0.361242)),
    (Data: (0.739009, 0, -0.673696)),
    (Data: (0.445738, 0, -0.895164)),
    (Data: (0.00838485, 0, 0.0251547)),
    (Data: (-0.273664, 0, -0.961825)),
    (Data: (-0.602635, 0, -0.798017)),
    (Data: (-0.850218, 0, -0.526432)),
    (Data: (-0.982973, 0, -0.183749)),
    (Data: (-0.982973, 0, 0.18375)),
    (Data: (-0.850217, 0, 0.526433)),
    (Data: (-0.602634, 0, 0.798018)),

    // (Data: (-0.850218, 0, -0.526432)),
    // (Data: (-0.982973, 0, -0.183749)),
    // (Data: (-0.982973, 0, 0.18375)),
    // (Data: (-0.850217, 0, 0.526433)),
    // (Data: (-0.602634, 0, 0.798018)),

    (Data: (-0.273663, 0, 0.961826)),
    (Data: (0.0922688, 0, 0.995734)),
    (Data: (0.445739, 0, 0.895163)),
    (Data: (0.739009, 0, 0.673695)),
    (Data: (0.932472, 0, 0.361241)),
    (Data: (1, 0, -2.44921e-016))
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

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleTriangulate);
{$endif}
end.
