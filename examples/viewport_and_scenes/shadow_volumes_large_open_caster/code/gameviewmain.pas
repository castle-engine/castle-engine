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

{ Loading a large mesh that is not 2-manifold, in a viewport using
  shadow volumes, with CastShadows = true (the default).

  The design (data/gameviewmain.castle-user-interface) has a point light
  with shadows (shadow volumes), a ground plane, a box casting a shadow
  and an empty SceneWall (CastShadows = true, the default).
  Here we load into SceneWall a wall of many separate tiles,
  with gaps between them. Like typical level geometry, it is not 2-manifold:
  all its edges are border edges.

  Then we measure how long MainViewport.PrepareResources takes for it.
  Preparing shadow volumes calculates the manifold and border edges
  of the mesh. The engine needs them even though the wall doesn't end up
  casting a shadow: shapes with border edges don't render shadow volumes
  (unless RenderOptions.WholeSceneManifold), and the border edges are how
  the engine knows it. }
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
    SceneWall: TCastleScene;
    LabelInfo, LabelFps: TCastleLabel;
  private
    { Tiles along each side of the wall. }
    TilesPerSide: Integer;
    { Seconds MainViewport.PrepareResources took for SceneWall. }
    PrepareTime: Double;
    ManifoldEdges, BorderEdges: Cardinal;
    function BuildWall: TX3DRootNode;
    procedure LoadWall;
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
  CastleUtils, CastleTimeUtils, CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

function TViewMain.BuildWall: TX3DRootNode;
const
  WallSize = 8;
var
  Points: TVector3List;
  Indexes: TInt32List;
  X, Y, Base: Integer;
  X0, Y0, Step, TileSize: Single;
  Coord: TCoordinateNode;
  Geometry: TIndexedTriangleSetNode;
  Material: TPhysicalMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
begin
  Step := WallSize / TilesPerSide;
  { gaps between tiles (a third of the step), so their shadows make dots }
  TileSize := Step * 2 / 3;
  Points := TVector3List.Create;
  Indexes := TInt32List.Create;
  try
    for X := 0 to TilesPerSide - 1 do
      for Y := 0 to TilesPerSide - 1 do
      begin
        { Each tile has its own vertexes, and there's a gap between tiles,
          so no edge is shared: every edge is a border edge. }
        X0 := X * Step - WallSize / 2;
        Y0 := Y * Step;
        Base := Points.Count;
        Points.Add(Vector3(X0           , Y0           , 0));
        Points.Add(Vector3(X0 + TileSize, Y0           , 0));
        Points.Add(Vector3(X0 + TileSize, Y0 + TileSize, 0));
        Points.Add(Vector3(X0           , Y0 + TileSize, 0));
        Indexes.AddRange([Base, Base + 1, Base + 2, Base, Base + 2, Base + 3]);
      end;

    Coord := TCoordinateNode.Create;
    Coord.SetPoint(Points);
    Geometry := TIndexedTriangleSetNode.Create;
    Geometry.Coord := Coord;
    Geometry.SetIndex(Indexes);
    { visible from both sides }
    Geometry.Solid := false;
  finally
    FreeAndNil(Points);
    FreeAndNil(Indexes);
  end;

  Material := TPhysicalMaterialNode.Create;
  Material.BaseColor := Vector3(0.8, 0.6, 0.3);
  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  Result := TX3DRootNode.Create;
  Result.AddChildren(Shape);
end;

procedure TViewMain.LoadWall;
var
  TimeStart: TTimerResult;
begin
  SceneWall.Load(BuildWall, true);

  { Prepare the scene for rendering now, measuring how long it takes.
    Since the viewport uses shadow volumes (PointLight1.Shadows = true)
    and the wall casts shadows, this includes preparing shadow volumes
    (prShadowVolume): the manifold and border edges of the wall. }
  TimeStart := Timer;
  MainViewport.PrepareResources(SceneWall);
  PrepareTime := TimerSeconds(Timer, TimeStart);

  SceneWall.EdgesCount(ManifoldEdges, BorderEdges);

  WritelnLog('Wall with %d triangles: PrepareResources took %f seconds, %d manifold edges, %d border edges', [
    SceneWall.TrianglesCount,
    PrepareTime,
    ManifoldEdges,
    BorderEdges
  ]);
  UpdateInfo;
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption := Format(
    'Wall: %d x %d separate tiles, %d triangles, CastShadows = true (the default).' + NL +
    'Not 2-manifold: %d manifold edges (tile diagonals), %d border edges,' + NL +
    'so it doesn''t render shadow volumes (only the box does).' + NL +
    'Preparing the wall (with its shadow volume edges) took %f seconds.' + NL +
    'Press + / - to change the number of tiles, R to load again.', [
    TilesPerSide,
    TilesPerSide,
    SceneWall.TrianglesCount,
    ManifoldEdges,
    BorderEdges,
    PrepareTime
  ]);
end;

procedure TViewMain.Start;
begin
  inherited;
  TilesPerSide := 100;
  LoadWall;
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

  if Event.IsKey(keyR) then
  begin
    LoadWall;
    Exit(true);
  end;

  if Event.IsKey(keyPlus) or Event.IsKey(keyNumpadPlus) then
  begin
    TilesPerSide := TilesPerSide + 50;
    LoadWall;
    Exit(true);
  end;

  if (Event.IsKey(keyMinus) or Event.IsKey(keyNumpadMinus)) and (TilesPerSide > 50) then
  begin
    TilesPerSide := TilesPerSide - 50;
    LoadWall;
    Exit(true);
  end;
end;

end.
