{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Express terrain as TCastleScene. }
unit TerrainScene;

interface

uses Classes,
  CastleVectors, CastleTerrain, CastleScene, X3DNodes;

type
  TTerrainScene = class(TCastleScene)
  private
    Geometry: TIndexedTriangleStripSetNode;
    CoordNode: TCoordinateNode;
    NormalNode: TNormalNode;
    ColorNode: TColorNode;
  public
    constructor Create(AOwner: TComponent); override;

    { Regenerate geometry (vertexes, normals, colors etc.) to show the current
      Terrain instance. This only calls @link(TTerrain.Height) method.
      BaseSize * 2 is the size of the square layer around the (MiddleX, MiddleY).

      TODO: This does not use TTerrain.Node, to maximize the speed of updating.
      Eventually, we would like to merge these two approaches (TTerrain.Node
      and TTerrainScene.Regenerate) as they really do the same: convert
      TTerrain height information into X3D nodes. }
    procedure Regenerate(Terrain: TTerrain;
      const Subdivision: Cardinal;
      MiddleX, MiddleY: Single; BaseSize: Single);
  end;

{ Returns same colors as used by @link(TTerrainScene.Regenerate). }
function ColorFromHeight(Terrain: TTerrain; Height: Single): TVector3;

implementation

uses SysUtils, CastleUtils;

constructor TTerrainScene.Create(AOwner: TComponent);
var
  Shape: TShapeNode;
  Root: TX3DRootNode;
begin
  inherited;

  Geometry := TIndexedTriangleStripSetNode.Create;

  CoordNode := TCoordinateNode.Create;
  Geometry.Coord := CoordNode;

  NormalNode := TNormalNode.Create;
  Geometry.Normal := NormalNode;

  ColorNode := TColorNode.Create;
  Geometry.Color := ColorNode;

  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);

  Load(Root, true);
end;

function ColorFromHeightCore(const H: Single): TVector3;
begin
  { Colors strategy from http://www.ii.uni.wroc.pl/~anl/dyd/PGK/pracownia.html }
  if      (H < 0  )  then Result := Vector3(0,       0,         1) { blue }
  else if (H < 500)  then Result := Vector3(0,       H/500,     0) { green }
  else if (H < 1000) then Result := Vector3(H/500-1, 1,         0) { yellow }
  else if (H < 1500) then Result := Vector3(1,       H/500-2.0, 0) { red }
  else Result := Vector3(1, 1, 1);                                 { white }
end;

function ColorFromHeight(Terrain: TTerrain; Height: Single): TVector3;
begin
  if Terrain is TTerrainGrid then
  begin
    { For TTerrainGrid, Height is original GridHeight result. }
    Height /= TTerrainGrid(Terrain).GridHeightScale;
  end else
  begin
    { scale height down by Amplitude, to keep nice colors regardless of Amplitude }
    if Terrain is TTerrainNoise then
      Height /= TTerrainNoise(Terrain).Amplitude;
    { some hacks to hit interesting colors }
    Height := Height  * 2000 - 1000;
  end;

  Result := ColorFromHeightCore(Height);
end;

procedure TTerrainScene.Regenerate(Terrain: TTerrain;
  const Subdivision: Cardinal;
  MiddleX, MiddleY: Single; BaseSize: Single);
var
  CountSteps, CountSteps1: Cardinal;
  X1, Y1, X2, Y2: Single;
  Coord, Color, Normal: TVector3List;
  Index: TLongIntList;

  procedure CalculatePositionColor(const I, J: Cardinal; out Position, Color: TVector3);
  begin
    { set XY to cover (X1, Y1) ... (X2, Y2) rectangle with our terrain }
    Position[0] := (X2 - X1) * I / (CountSteps-1) + X1;
    Position[1] := (Y2 - Y1) * J / (CountSteps-1) + Y1;
    Position[2] := Terrain.Height(Position[0], Position[1]);

    Color := ColorFromHeight(Terrain, Position[2]);
  end;

  procedure CalculateNormal(const I, J: Cardinal; out Normal: TVector3);
  var
    P, PX, PY: PVector3;
  begin
    P  := Coord.Ptr( I      * CountSteps1 + J);
    PX := Coord.Ptr((I + 1) * CountSteps1 + J);
    PY := Coord.Ptr( I      * CountSteps1 + J + 1);

    { TODO: this is actually normal vector of 1 of the four faces around this
      vertex. Optimally, we should calculate normals on all faces,
      and for vertex normal take average. }
    Normal := TVector3.CrossProduct(
      (PX^ - P^),
      (PY^ - P^));
  end;

const
  RoundGridCell = 0.5;
var
  I, J, Idx: Cardinal;
  IndexPtr: PLongInt;
begin
  { to somewhat cure the effect of terrain "flowing" (because every small
    change of Middle point shifts all the points), round middle to
    some cell size. }
  MiddleX := Round(MiddleX / RoundGridCell) * RoundGridCell;
  MiddleY := Round(MiddleY / RoundGridCell) * RoundGridCell;

  X1 := MiddleX - BaseSize;
  Y1 := MiddleY - BaseSize;
  X2 := MiddleX + BaseSize;
  Y2 := MiddleY + BaseSize;

  { CountSteps-1 squares (edges) along the way,
    CountSteps points along the way.
    Calculate positions for CountSteps + 1 points
    (+ 1 additional for normal calculation). }
  CountSteps := 1 shl Subdivision + 1;
  CountSteps1 := CountSteps + 1;

  Index := Geometry.FdIndex.Items;
  Coord := CoordNode.FdPoint.Items;
  Normal := NormalNode.FdVector.Items;
  Color := ColorNode.FdColor.Items;

  { We will render CountSteps^2 points, but we want to calculate
    (CountSteps + 1)^2 points : to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  Coord.Count := Sqr(CountSteps1);
  Normal.Count := Sqr(CountSteps1);
  Color.Count := Sqr(CountSteps1);

  { calculate Points and Colors }
  for I := 0 to CountSteps do
    for J := 0 to CountSteps do
    begin
      Idx := I * CountSteps1 + J;
      CalculatePositionColor(I, J, Coord.List^[Idx], Color.List^[Idx]);
    end;
  CoordNode.FdPoint.Changed;
  ColorNode.FdColor.Changed;

  { calculate Normals }
  for I := 0 to CountSteps - 1 do
    for J := 0 to CountSteps - 1 do
    begin
      Idx := I * CountSteps1 + J;
      CalculateNormal(I, J, Normal.List^[Idx]);
    end;
  NormalNode.FdVector.Changed;

  { calculate Index }
  Index.Count := (CountSteps - 1) * (CountSteps * 2 + 1);
  IndexPtr := PLongInt(Index.List);
  for I := 1 to CountSteps - 1 do
  begin
    for J := 0 to CountSteps - 1 do
    begin
      IndexPtr^ := (I - 1) * CountSteps1 + J; Inc(IndexPtr);
      IndexPtr^ :=  I      * CountSteps1 + J; Inc(IndexPtr);
    end;
    IndexPtr^ := -1;
    Inc(IndexPtr);
  end;
  Geometry.FdIndex.Changed;
end;

end.
