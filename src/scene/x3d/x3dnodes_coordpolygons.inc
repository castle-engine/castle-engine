{%MainUnit x3dnodes.pas}

type
  { Common utility class for nodes that implement InternalCoordPolygons
    by calling MakeCoordRanges,
    and then doing the work inside HandleCoordRange. }
  TCoordRangePolygonizer = class
    Polygon: TIndexedPolygonHandler;

    { This should be overridden to call Polygon procedure. }
    procedure HandleCoordRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); virtual; abstract;
  end;

{ TriangleFanSet ------------------------------------------------------------- }

type
  TTriangleFanSetPolygonizer = class(TCoordRangePolygonizer)
    procedure HandleCoordRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

procedure TTriangleFanSetPolygonizer.HandleCoordRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
var
  FirstIndex: Integer;
begin
  FirstIndex := BeginIndex;

  while BeginIndex + 2 < EndIndex do
  begin
    Polygon([FirstIndex, BeginIndex + 1, BeginIndex + 2]);
    Inc(BeginIndex);
  end;
end;

procedure TIndexedTriangleFanSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TTriangleFanSetPolygonizer;
begin
  Polygonizer := TTriangleFanSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

procedure TTriangleFanSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TTriangleFanSetPolygonizer;
begin
  Polygonizer := TTriangleFanSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

{ TriangleStripSet ----------------------------------------------------------- }

type
  TTriangleStripSetPolygonizer = class(TCoordRangePolygonizer)
    procedure HandleCoordRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

procedure TTriangleStripSetPolygonizer.HandleCoordRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
var
  NormalOrder: boolean;
begin
  NormalOrder := true;

  while BeginIndex + 2 < EndIndex do
  begin
    if NormalOrder then
      Polygon([BeginIndex    , BeginIndex + 1, BeginIndex + 2]) else
      Polygon([BeginIndex + 1, BeginIndex    , BeginIndex + 2]);
    Inc(BeginIndex);
    NormalOrder := not NormalOrder;
  end;
end;

procedure TIndexedTriangleStripSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TTriangleStripSetPolygonizer;
begin
  Polygonizer := TTriangleStripSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

procedure TTriangleStripSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TTriangleStripSetPolygonizer;
begin
  Polygonizer := TTriangleStripSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

procedure TIndexedTriangleMeshNode_1.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TTriangleStripSetPolygonizer;
begin
  Polygonizer := TTriangleStripSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

{ TriangleSet ---------------------------------------------------------------- }

procedure TriangleSetPolygonizer(
  const CoordCount: Cardinal;
  PolygonHandler: TIndexedPolygonHandler);
var
  I: Cardinal;
begin
  I := 0;
  while I + 2 < CoordCount do
  begin
    PolygonHandler([I, I + 1, I + 2]);
    Inc(I, 3);
  end;
end;

procedure TIndexedTriangleSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
begin
  if InternalCoordinates(State) <> nil then
    TriangleSetPolygonizer(CoordIndexField.Count, PolygonHandler);
end;

procedure TTriangleSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  ACoord: TMFVec3f;
begin
  ACoord := InternalCoordinates(State);
  if ACoord <> nil then
    TriangleSetPolygonizer(ACoord.Count, PolygonHandler);
end;

{ QuadSet ---------------------------------------------------------------- }

procedure QuadSetPolygonizer(
  const CoordCount: Cardinal;
  PolygonHandler: TIndexedPolygonHandler);
var
  I: Cardinal;
begin
  I := 0;
  while I + 3 < CoordCount do
  begin
    PolygonHandler([I, I + 1, I + 2, I + 3]);
    Inc(I, 4);
  end;
end;

procedure TIndexedQuadSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
begin
  if InternalCoordinates(State) <> nil then
    QuadSetPolygonizer(CoordIndexField.Count, PolygonHandler);
end;

procedure TQuadSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  ACoord: TMFVec3f;
begin
  ACoord := InternalCoordinates(State);
  if ACoord <> nil then
    QuadSetPolygonizer(ACoord.Count, PolygonHandler);
end;

{ TriangleFanSet ------------------------------------------------------------- }

type
  TIndexedFaceSetPolygonizer = class(TCoordRangePolygonizer)
    procedure HandleCoordRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

procedure TIndexedFaceSetPolygonizer.HandleCoordRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
var
  Indexes: array of Cardinal;
  I: Cardinal;
begin
  SetLength(Indexes, EndIndex - BeginIndex);
  if High(Indexes) >= 0 then
    for I := 0 to High(Indexes) do
      Indexes[I] := I + Cardinal(BeginIndex);
  Polygon(Indexes);
end;

procedure TIndexedFaceSetNode_1.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TIndexedFaceSetPolygonizer;
begin
  Polygonizer := TIndexedFaceSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;

procedure TIndexedFaceSetNode.InternalCoordPolygons(
  State: TX3DGraphTraverseState;
  PolygonHandler: TIndexedPolygonHandler);
var
  Polygonizer: TIndexedFaceSetPolygonizer;
begin
  Polygonizer := TIndexedFaceSetPolygonizer.Create;
  try
    Polygonizer.Polygon := PolygonHandler;
    InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif} Polygonizer.HandleCoordRange);
  finally FreeAndNil(Polygonizer) end;
end;
