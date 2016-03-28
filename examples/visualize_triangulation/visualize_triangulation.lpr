{
  Copyright 2011-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualize how triangulation of (potentially concave) polygon works.
  This results in a series of images showing how the polygon looks like
  (how vertexes are ordered), and then how the triangulation progresses.
  If the polygon is self-intersecting (we do *not* guarantee handling
  it correctly and VRML/X3D do *not* allow it), this program should allow
  you to clearly see it.

  It shows vertexes in order (to better understand the other debug messages
  in console), then shows created triangles (with the important edge vectors
  E1,E2,E3).

  Also, a lot of log information about the triangulation is printed
  (on StdErr).

  The polygon is extracted from the crossSection of the 1st Extrusion
  node of the VRML/X3D file (since that's where complicated concave
  polygons are most often used). Alternatively, just define a series of
  polygon points in code below, see array Polygon below.
}

uses SysUtils, Classes, CastleStringUtils, CastleLog, CastleParameters,
  FpImage, FpCanvas, FpImgCanv, FpWritePNG, CastleUtils, CastleGraphUtil,
  CastleTriangulate, CastleVectors, CastleTriangles, CastleSceneCore,
  CastleURIUtils, X3DNodes;

type
  { Do visualize triangulation of a single polygon. }
  TVisualizeTriangulation = class
  strict private
    TriangleCount: Integer;
    Vertexes: TVector3SingleList;
    ImageUrlPrefix: string;
    Image: TFPCustomImage;
    Canvas: TFPCustomCanvas;
    VisualizeX, VisualizeY: Cardinal;
    MinV, MaxV: TVector3Single;

    function VisualizePoint(const P: TVector3Single): TPoint;
    function VisualizePointRect(const P: TVector3Single): TRect;
    procedure SaveImage(const Url, Message: string);
    procedure Face(const Tri: TVector3Longint);
    procedure CreateCommon(
      const Name: string; AVisualizeX, AVisualizeY: Cardinal;
      const RevertOrder: boolean);
  public
    constructor Create(AVertexes: array of TVector3Single;
      const Name: string; AVisualizeX, AVisualizeY: Cardinal;
      const RevertOrder: boolean);
    constructor Create(const URL: string);
    destructor Destroy; override;
    procedure VisualizePolygon;
    procedure VisualizeTriangulation;
  end;

function TVisualizeTriangulation.VisualizePoint(const P: TVector3Single): TPoint;
begin
  Result := Point(
    Round(MapRange(P[VisualizeX], MinV[VisualizeX], MaxV[VisualizeX], 0, Image.Width)),
    Round(MapRange(P[VisualizeY], MinV[VisualizeY], MaxV[VisualizeY], 0, Image.Height))
  );
end;

function TVisualizeTriangulation.VisualizePointRect(const P: TVector3Single): TRect;
var
  Pt: TPoint;
begin
  Pt := VisualizePoint(P);
  Result := Rect(Pt.X - 10, Pt.Y - 10, Pt.X + 10, Pt.Y + 10);
end;

procedure TVisualizeTriangulation.SaveImage(const Url, Message: string);
var
  Writer: TFPWriterPNG;
begin
  { recreate Writer each time, to workaround http://bugs.freepascal.org/view.php?id=21840 }
  Writer := TFPWriterPNG.Create;
  try
    Writer.Indexed := false;
    Image.SaveToFile(Url, Writer);
  finally FreeAndNil(Writer) end;

  Writeln(Message, ' (Saved to ', Url, ')');
end;

procedure TVisualizeTriangulation.Face(const Tri: TVector3Longint);
var
  V0, V1, V2, EarNormal: TVector3Single;
  E1, E2, E3: TVector3Single;
  Middle: TPoint;
begin
  Inc(TriangleCount);

  V0 := Vertexes[Tri[0]];
  V1 := Vertexes[Tri[1]];
  V2 := Vertexes[Tri[2]];

  { Calculate (and possibly visualize later) vectors E1, E2, E3 exactly
    like the ones calculated in TriangulateFace algorithm. }
  EarNormal := TriangleDir(V0, V1, V2);
  Assert(not ZeroVector(EarNormal));
  NormalizeVar(EarNormal);

  E1 := VectorProduct(EarNormal, V0 - V1);
  E2 := VectorProduct(EarNormal, V1 - V2);
  E3 := VectorProduct(EarNormal, V2 - V0);

  { draw triangle, each triangle with different (random) color }
  Canvas.Pen.FPColor := RandomLightFPColor;
  Canvas.Pen.Width := 5;
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
  Canvas.Pen.Width := 3;
  Canvas.PolyLine([VisualizePoint((V0 + V1) / 2.0),
                   VisualizePoint((V0 + V1) / 2.0 + E1 / 10.0)]);
  Canvas.PolyLine([VisualizePoint((V1 + V2) / 2.0),
                   VisualizePoint((V1 + V2) / 2.0 + E2 / 10.0)]);
  Canvas.PolyLine([VisualizePoint((V2 + V0) / 2.0),
                   VisualizePoint((V2 + V0) / 2.0 + E3 / 10.0)]);

  SaveImage(Format(ImageUrlPrefix + '_%d.png', [TriangleCount]),
    Format('Triangle %d: %d - %d - %d', [TriangleCount, Tri[0], Tri[1], Tri[2]]));
end;

constructor TVisualizeTriangulation.Create(const URL: string);
var
  Scene: TCastleSceneCore;
  Extrusion: TExtrusionNode;
  I: Integer;
begin
  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.URL := URL;
    Extrusion := Scene.RootNode.TryFindNode(TExtrusionNode, false) as TExtrusionNode;
    if Extrusion = nil then
      raise Exception.CreateFmt('No Extrusion node found in scene "%s"', [URL]);

    { initialize Vertexes based on Extrusion.crossSection }
    Vertexes := TVector3SingleList.Create;
    Vertexes.Count := Extrusion.FdCrossSection.Count;
    for I := 0 to Vertexes.Count - 1 do
      Vertexes.List^[I] := Extrusion.CrossSection3D(I);
  finally FreeAndNil(Scene) end;

  CreateCommon(ExtractURIName(URL), 0, 2, false);
end;

constructor TVisualizeTriangulation.Create(AVertexes: array of TVector3Single;
  const Name: string; AVisualizeX, AVisualizeY: Cardinal;
  const RevertOrder: boolean);
var
  I: Integer;
begin
  inherited Create;

  { initialize Vertexes based on array AVertexes }
  Vertexes := TVector3SingleList.Create;
  Vertexes.Count := High(AVertexes) + 1;
  for I := 0 to Vertexes.Count - 1 do
    Vertexes.List^[I] := AVertexes[I];

  CreateCommon(Name, AVisualizeX, AVisualizeY, RevertOrder);
end;

procedure TVisualizeTriangulation.CreateCommon(
  const Name: string; AVisualizeX, AVisualizeY: Cardinal;
  const RevertOrder: boolean);
var
  I: Integer;
begin
  TriangleCount := 0;

  if RevertOrder then
    for I := 0 to Vertexes.Count div 2 - 1 do
      SwapValues(Vertexes.List^[I], Vertexes.List^[Vertexes.Count - 1 - I]);

  Image := TFPMemoryImage.Create(1024, 1024);
  Image.UsePalette := false;
  { TFPImageCanvas has some abstract methods not overridden.
    This is FPC bug in FpImgCanv. Ignore following warnings. }
  {$warnings off}
  Canvas := TFPImageCanvas.Create(Image);
  {$warnings on}

  ImageUrlPrefix := SUnformattable(Name);
  if RevertOrder then
    ImageUrlPrefix += '_reverted';
  VisualizeX := AVisualizeX;
  VisualizeY := AVisualizeY;

  { calculate MinV/MaxV to include all Vertexes, to show whole polygon }
  MinV := Vertexes[0];
  MaxV := Vertexes[0];
  for I := 1 to Vertexes.Count - 1 do
  begin
    MinVar(MinV[0], Vertexes[I][0]);
    MinVar(MinV[1], Vertexes[I][1]);
    MinVar(MinV[2], Vertexes[I][2]);
    MaxVar(MaxV[0], Vertexes[I][0]);
    MaxVar(MaxV[1], Vertexes[I][1]);
    MaxVar(MaxV[2], Vertexes[I][2]);
  end;

  { make MinV/MaxV even slightly more distant, to have some margin around
    visualized polygon }
  for I := 0 to 2 do
  begin
    MinV[I] -= (MaxV[I] - MinV[I]) / 10;
    MaxV[I] += (MaxV[I] - MinV[I]) / 10;
  end;
end;

procedure TVisualizeTriangulation.VisualizePolygon;
var
  I: Integer;
begin
  Canvas.Pen.FPColor := FPColor($FFFF, $FFFF, $FFFF);

  Canvas.MoveTo(VisualizePoint(Vertexes[0]));
  Canvas.Brush.FPColor := RandomLightFPColor;
  Canvas.Ellipse(VisualizePointRect(Vertexes[0]));
  SaveImage(Format(ImageUrlPrefix + '_0_%d.png', [0]),
    Format('Vertex %d', [0]));
  for I := 1 to Vertexes.Count - 1 do
  begin
    Canvas.LineTo(VisualizePoint(Vertexes[I]));
    Canvas.Brush.FPColor := RandomLightFPColor;
    Canvas.Ellipse(VisualizePointRect(Vertexes[I]));
    SaveImage(Format(ImageUrlPrefix + '_0_%d.png', [I]),
      Format('Vertex %d', [I]));
  end;
  Canvas.LineTo(VisualizePoint(Vertexes[0]));
end;

procedure TVisualizeTriangulation.VisualizeTriangulation;
begin
  TriangulateFace(nil, Vertexes.Count, Vertexes.L, Vertexes.Count, @Face, 0);
end;

destructor TVisualizeTriangulation.Destroy;
begin
  FreeAndNil(Vertexes);
  FreeAndNil(Image);
  FreeAndNil(Canvas);
  inherited;
end;

{ global routines ------------------------------------------------------------ }

const
  { Hardcoded polygon from https://sourceforge.net/p/castle-engine/tickets/13/ }
  Polygon: array [0..17] of TVector3Single = (
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
    (-0.273663, 0, 0.961826),
    (0.0922688, 0, 0.995734),
    (0.445739, 0, 0.895163),
    (0.739009, 0, 0.673695),
    (0.932472, 0, 0.361241),
    (1, 0, -2.44921e-016)
  );

var
  URL: string;
  Vis: TVisualizeTriangulation;
begin
  InitializeLog('1.0.0');
  LogTriangulation := true;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    URL := Parameters[1] else
    URL := '';

  if URL <> '' then
    Vis := TVisualizeTriangulation.Create(URL) else
    { If no URL given, just visualize hardcoded Polygon array. }
    Vis := TVisualizeTriangulation.Create(Polygon, 'Polygon', 0, 2, false);
  try
    Vis.VisualizePolygon;
    Vis.VisualizeTriangulation;
  finally FreeAndNil(Vis) end;
end.
