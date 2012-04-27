{
  Copyright 2011-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ }
unit TestTriangulator;

{ Besides doing some tests, we can also make a visualization of triangulation.
  This results in a series of images showing how triangulation progresses,
  hopefully useful to debug TriangulateFace problems.

  It shows vertexes in order (to better understand the other debug messages
  in console), then shows created triangles (with the important edge vectors
  E1,E2,E3).
  For full debug info, add -dVISUALIZE_TRIANGULATION to ~/.fpc.cfg
  and recompile both this unit and 3d/triangulator.pas unit.

  Define the symbol below, and run the tests to get the images.
  Console will contain messages about where the images are written,
  and how the triangulation progresses.
  For better debugging, you may also want to:
  - Change beginning test_castle_game_engine.lpr to run only TestTriangulateFace
  - Change TestTriangulateFace implementation to run only one DoPolygon,
    the one that interests you.
}
{ $define VISUALIZE_TRIANGULATION}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, VectorMath, Triangulator,
  FpImage, FpCanvas, FpImgCanv, FpWritePNG;

type
  TTestTriangulator = class(TTestCase)
  private
    { private vars for Face callback }
    Vertexes: PVector3Single;
    CountVertexes: Integer;
    TriangleCount: Cardinal;
    {$ifdef VISUALIZE_TRIANGULATION}
    ImageFileNamePrefix: string;
    Image: TFPCustomImage;
    Canvas: TFPCustomCanvas;
    VisualizeX, VisualizeY: Cardinal;
    MinV, MaxV: TVector3Single;
    function VisualizePoint(const P: TVector3Single): TPoint;
    function VisualizePointRect(const P: TVector3Single): TRect;
    procedure SaveImage(const FileName, Message: string);
    {$endif VISUALIZE_TRIANGULATION}
    procedure Face(const Tri: TVector3Longint);
  published
    procedure TestIndexedConcavePolygonNormal;
    procedure TestTriangulateFace;
  end;

implementation

uses CastleStringUtils, CastleUtils, CastleGraphUtil;

procedure TTestTriangulator.TestIndexedConcavePolygonNormal;
const
  Verts: array [0..3] of TVector3Single =
  ( (-1, 0, 0),
    (0, 1, 0),
    (1, -1, 0),
    (0, 0, 0)
  );
  Indexes: array [0..3] of LongInt = (0, 1, 2, 3);
begin
  Assert(VectorsEqual(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single, false),
    Vector3Single(0, 0, -1)));

  { This is an example polygon that cannot be handled
    by IndexedConvexPolygonNormal }
  Assert(not VectorsEqual(
    IndexedConvexPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single),
    Vector3Single(0, 0, -1)));
  Assert(not VectorsEqual(
    IndexedPolygonNormal(@Indexes, High(Indexes) + 1,
      @Verts, High(Verts) + 1, ZeroVector3Single, true),
    Vector3Single(0, 0, -1)));
end;

{$ifdef VISUALIZE_TRIANGULATION}
function TTestTriangulator.VisualizePoint(const P: TVector3Single): TPoint;
begin
  Result := Point(
    Round(MapRange(P[VisualizeX], MinV[VisualizeX], MaxV[VisualizeX], 0, Image.Width)),
    Round(MapRange(P[VisualizeY], MinV[VisualizeY], MaxV[VisualizeY], 0, Image.Height))
  );
end;

function TTestTriangulator.VisualizePointRect(const P: TVector3Single): TRect;
var
  Pt: TPoint;
begin
  Pt := VisualizePoint(P);
  Result := Rect(Pt.X - 10, Pt.Y - 10, Pt.X + 10, Pt.Y + 10);
end;

procedure TTestTriangulator.SaveImage(const FileName, Message: string);
var
  Writer: TFPWriterPNG;
begin
  { recreate Writer each time, to workaround http://bugs.freepascal.org/view.php?id=21840 }
  Writer := TFPWriterPNG.Create;
  try
    Writer.Indexed := false;
    Image.SaveToFile(FileName, Writer);
  finally FreeAndNil(Writer) end;

  Writeln(Message, ' (Saved to ', FileName, ')');
end;
{$endif VISUALIZE_TRIANGULATION}

procedure TTestTriangulator.Face(const Tri: TVector3Longint);
var
  V0, V1, V2, EarNormal, E1, E2, E3: TVector3Single;
  Middle: TPoint;
begin
  Inc(TriangleCount);

  V0 := Vertexes[Tri[0]];
  V1 := Vertexes[Tri[1]];
  V2 := Vertexes[Tri[2]];

  { Calculate (and possibly visualize later) vectors E1, E2, E3 exactly
    like the ones calculated in TriangulateFace algorithm. }
  EarNormal := TriangleDir(V0, V1, V2);
  // TODO: for now, it's possible to get zero normals, see TODO in Triangulator
//  Assert(not ZeroVector(EarNormal));
  NormalizeTo1st(EarNormal);

  E1 := VectorProduct(EarNormal, V0 - V1);
  E2 := VectorProduct(EarNormal, V1 - V2);
  E3 := VectorProduct(EarNormal, V2 - V0);

  {$ifdef VISUALIZE_TRIANGULATION}
  { draw triangle, each triangle with different (random) color }
  Canvas.Pen.FPColor := FPColor(Random($FFFF), Random($FFFF), Random($FFFF));
  Canvas.Pen.Width := 3;
  { We would prefer to just use Canvas.Polygon (with brush) to draw triangle,
    but it's not implemented. Also Canvas.FloodFill is not available.
    So we draw triangle outline only with PolyLine, then use simple FloodFill
    from CastleGraphUtil. }
  Canvas.PolyLine([VisualizePoint(V0),
                   VisualizePoint(V1),
                   VisualizePoint(V2),
                   VisualizePoint(V0)]);
  Middle := VisualizePoint((V0 + V1 + V2) / 3.0);
  Canvas.Brush.FPColor := Canvas.Pen.FPColor;
  FloodFill(Canvas, Middle.X, Middle.Y, Canvas.Pen.FPColor, fsBorder);
  { Draw E1, E2, E3 vectors }
  Canvas.Pen.Width := 1;
  Canvas.PolyLine([VisualizePoint((V0 + V1) / 2.0),
                   VisualizePoint((V0 + V1) / 2.0 + E1 / 10.0)]);
  Canvas.PolyLine([VisualizePoint((V1 + V2) / 2.0),
                   VisualizePoint((V1 + V2) / 2.0 + E2 / 10.0)]);
  Canvas.PolyLine([VisualizePoint((V2 + V0) / 2.0),
                   VisualizePoint((V2 + V0) / 2.0 + E3 / 10.0)]);

  SaveImage(Format(ImageFileNamePrefix + '_%d.png', [TriangleCount]),
    Format('Triangle %d: %d - %d - %d', [TriangleCount, Tri[0], Tri[1], Tri[2]]));
  {$endif VISUALIZE_TRIANGULATION}
end;

procedure TTestTriangulator.TestTriangulateFace;

  procedure DoPolygon(AVertexes: array of TVector3Single;
    const Name: string;
    AVisualizeX, AVisualizeY: Cardinal; const RevertOrder: boolean);
  var
    I: Integer;
  begin
    try
      Vertexes := @AVertexes;
      CountVertexes := High(AVertexes) + 1;
      TriangleCount := 0;

      if RevertOrder then
        for I := 0 to CountVertexes div 2 - 1 do
          SwapValues(Vertexes[I], Vertexes[CountVertexes - 1 - I]);

      {$ifdef VISUALIZE_TRIANGULATION}
      Image := TFPMemoryImage.Create(1024, 1024);
      Image.UsePalette := false;
      { TFPImageCanvas has some abstract methods not overridden.
        This is FPC bug in FpImgCanv. Ignore following warnings. }
      {$warnings off}
      Canvas := TFPImageCanvas.Create(Image);
      {$warnings on}

      ImageFileNamePrefix := SUnformattable(InclPathDelim(GetTempDir) + Name);
      if RevertOrder then
        ImageFileNamePrefix += '_reverted';
      VisualizeX := AVisualizeX;
      VisualizeY := AVisualizeY;

      { calculate MinV/MaxV to include all Vertexes, to show whole polygon }
      MinV := Vertexes[0];
      MaxV := Vertexes[0];
      for I := 1 to CountVertexes - 1 do
      begin
        MinTo1st(MinV[0], Vertexes[I][0]);
        MinTo1st(MinV[1], Vertexes[I][1]);
        MinTo1st(MinV[2], Vertexes[I][2]);
        MaxTo1st(MaxV[0], Vertexes[I][0]);
        MaxTo1st(MaxV[1], Vertexes[I][1]);
        MaxTo1st(MaxV[2], Vertexes[I][2]);
      end;

      { make MinV/MaxV even slightly more distant, to have some margin around
        visualized polygon }
      for I := 0 to 2 do
      begin
        MinV[I] -= (MaxV[I] - MinV[I]) / 10;
        MaxV[I] += (MaxV[I] - MinV[I]) / 10;
      end;

      Canvas.Pen.FPColor := FPColor($FFFF, $FFFF, $FFFF);
      Canvas.Brush.FPColor := FPColor($FFFF, $FFFF, $FFFF);

      Canvas.MoveTo(VisualizePoint(Vertexes[0]));
      Canvas.Ellipse(VisualizePointRect(Vertexes[0]));
      SaveImage(Format(ImageFileNamePrefix + '_0_%d.png', [0]),
        Format('Vertex %d', [0]));
      for I := 1 to CountVertexes - 1 do
      begin
        Canvas.LineTo(VisualizePoint(Vertexes[I]));
        Canvas.Ellipse(VisualizePointRect(Vertexes[I]));
        SaveImage(Format(ImageFileNamePrefix + '_0_%d.png', [I]),
          Format('Vertex %d', [I]));
      end;
      Canvas.LineTo(VisualizePoint(Vertexes[0]));
      {$endif VISUALIZE_TRIANGULATION}

      TriangulateFace(nil, CountVertexes, Vertexes, @Face, 0);
    finally
      {$ifdef VISUALIZE_TRIANGULATION}
      FreeAndNil(Image);
      FreeAndNil(Canvas);
      {$endif VISUALIZE_TRIANGULATION}
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

  Polygon_R3D_cs_full_polygon: array [0..39] of TVector3Single = (
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
begin
  DoPolygon(Polygon_3_5, 'polygon_3_5', 0, 2, false);
  DoPolygon(Polygon_R3D_cs, 'R3D_cs', 0, 2, false);
  DoPolygon(Polygon_R3D_cs_full_polygon, 'R3D_cs_full_polygon', 0, 2, false);
  { TODO: test that results are same as hardcoded results }
  { TODO: maybe test that resulting edges do not cross each other
    or original polygon edges? But it's not so easy, as on non-trivial
    polygons we will have colinear edges. }
  { TODO: just test for both RevertOrder = true and false }
end;

initialization
  RegisterTest(TTestTriangulator);
end.
