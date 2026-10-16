{
  Copyright 2026 Popa Marius Adrian.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading a large scene that doesn't cast shadows, in a viewport
  using shadow volumes.

  The design (data/gameviewmain.castle-user-interface) has a point light
  with shadows (shadow volumes), a box casting a shadow,
  and an empty SceneLevel with CastShadows = false.
  Here we load into SceneLevel a "level": a floor of many separate tiles,
  with gaps between them. Like typical level geometry, it is not 2-manifold:
  all its edges are border edges.

  Then we measure how long MainViewport.PrepareResources takes for it.
  When the level doesn't cast shadows, preparing it should not calculate
  shadow volume edges, which for such a mesh takes a long time
  (the calculation is quadratic in the number of border edges). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleViewport,
  X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport: TCastleViewport;
    SceneLevel: TCastleScene;
    LabelInfo, LabelFps: TCastleLabel;
  private
    { Tiles along each side of the level floor. }
    TilesPerSide: Integer;
    LevelCastsShadows: Boolean;
    { Seconds MainViewport.PrepareResources took for SceneLevel. }
    PrepareTime: Double;
    { Shapes of SceneLevel with shadow volume edges calculated. }
    ShapesWithEdges, ShapesCount: Integer;
    function BuildLevel: TX3DRootNode;
    procedure LoadLevel;
    procedure UpdateInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleTimeUtils, CastleLog, CastleShapes, CastleColors;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

function TViewMain.BuildLevel: TX3DRootNode;
const
  TileSize = 0.18;
  Gap = 0.02;
var
  Points: TVector3List;
  Indexes: TInt32List;
  X, Z, Base: Integer;
  X0, Z0, Half: Single;
  Coord: TCoordinateNode;
  Geometry: TIndexedTriangleSetNode;
  Material: TPhysicalMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
begin
  Points := TVector3List.Create;
  Indexes := TInt32List.Create;
  try
    Half := TilesPerSide * (TileSize + Gap) / 2;
    for X := 0 to TilesPerSide - 1 do
      for Z := 0 to TilesPerSide - 1 do
      begin
        { Each tile has its own vertexes, and there's a gap between tiles,
          so no edge is shared: every edge is a border edge. }
        X0 := X * (TileSize + Gap) - Half;
        Z0 := Z * (TileSize + Gap) - Half;
        Base := Points.Count;
        Points.Add(Vector3(X0           , 0, Z0 + TileSize));
        Points.Add(Vector3(X0 + TileSize, 0, Z0 + TileSize));
        Points.Add(Vector3(X0 + TileSize, 0, Z0));
        Points.Add(Vector3(X0           , 0, Z0));
        Indexes.AddRange([Base, Base + 1, Base + 2, Base, Base + 2, Base + 3]);
      end;

    Coord := TCoordinateNode.Create;
    Coord.SetPoint(Points);
    Geometry := TIndexedTriangleSetNode.Create;
    Geometry.Coord := Coord;
    Geometry.SetIndex(Indexes);
  finally
    FreeAndNil(Points);
    FreeAndNil(Indexes);
  end;

  Material := TPhysicalMaterialNode.Create;
  Material.BaseColor := Vector3(0.6, 0.7, 0.8);
  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  Result := TX3DRootNode.Create;
  Result.AddChildren(Shape);
end;

procedure TViewMain.LoadLevel;
var
  TimeStart: TTimerResult;
  Shape: TShape;
begin
  SceneLevel.Load(BuildLevel, true);
  SceneLevel.CastShadows := LevelCastsShadows;

  { Prepare the scene for rendering now, measuring how long it takes.
    Since the viewport uses shadow volumes (PointLight1.Shadows = true),
    this includes preparing shadow volumes (prShadowVolume). }
  TimeStart := Timer;
  MainViewport.PrepareResources(SceneLevel);
  PrepareTime := TimerSeconds(Timer, TimeStart);

  ShapesCount := 0;
  ShapesWithEdges := 0;
  for Shape in SceneLevel.Shapes.TraverseList(false) do
  begin
    Inc(ShapesCount);
    if Shape.InternalShadowVolumes.ManifoldAndBorderEdgesCalculated then
      Inc(ShapesWithEdges);
  end;

  WritelnLog('Level with %d triangles, CastShadows = %s: PrepareResources took %f seconds, shadow volume edges calculated for %d / %d shapes', [
    SceneLevel.TrianglesCount,
    BoolToStr(LevelCastsShadows, true),
    PrepareTime,
    ShapesWithEdges,
    ShapesCount
  ]);
  UpdateInfo;
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption := Format(
    'Level: %d x %d separate tiles, %d triangles (not 2-manifold: all edges are border edges).' + NL +
    'Level CastShadows: %s (press C to toggle and load again).' + NL +
    'Preparing the level took %f seconds.' + NL +
    'Shadow volume edges calculated for %d / %d shapes of the level.' + NL +
    'Press + / - to change the number of tiles, R to load again.' + NL +
    'The box always casts a shadow.', [
    TilesPerSide,
    TilesPerSide,
    SceneLevel.TrianglesCount,
    BoolToStr(LevelCastsShadows, true),
    PrepareTime,
    ShapesWithEdges,
    ShapesCount
  ]);
end;

procedure TViewMain.Start;
begin
  inherited;
  TilesPerSide := 100;
  LevelCastsShadows := false;
  LoadLevel;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyC) then
  begin
    LevelCastsShadows := not LevelCastsShadows;
    LoadLevel;
    Exit(true);
  end;

  if Event.IsKey(keyR) then
  begin
    LoadLevel;
    Exit(true);
  end;

  if Event.IsKey(keyPlus) or Event.IsKey(keyNumpadPlus) then
  begin
    TilesPerSide := TilesPerSide + 25;
    LoadLevel;
    Exit(true);
  end;

  if (Event.IsKey(keyMinus) or Event.IsKey(keyNumpadMinus)) and (TilesPerSide > 25) then
  begin
    TilesPerSide := TilesPerSide - 25;
    LoadLevel;
    Exit(true);
  end;
end;

end.
