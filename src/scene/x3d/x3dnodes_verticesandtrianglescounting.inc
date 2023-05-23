{%MainUnit x3dnodes.pas}
{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ implementacja metod VerticesCount i TrianglesCount dla specyficznych
  potomkow TAbstractGeometryNode }

function TAbstractGeometryNode.CallProxyVerticesCount(
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := ProxyGeometry.VerticesCount(ProxyState, nil, nil);
end;

function TAbstractGeometryNode.VerticesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
var
  C: TMFVec3f;
begin
  if InternalCoord(State, C) then
  begin
    if C <> nil then
      Result := C.Count else
      Result := 0;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyVerticesCount(ProxyGeometry, ProxyState) else
      raise EInternalError.CreateFmt('%s: TAbstractGeometryNode.VerticesCount not overridden, and node not coordinate-based and without a Proxy', [ClassName]);
  end;
end;

function TAbstractGeometryNode.CallProxyTrianglesCount(
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := ProxyGeometry.TrianglesCount(ProxyState, nil, nil);
end;

function TAbstractGeometryNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if ProxyGeometry <> nil then
    Result := CallProxyTrianglesCount(ProxyGeometry, ProxyState) else
    raise EInternalError.CreateFmt('%s: TAbstractGeometryNode.TrianglesCount not overridden, and node without a Proxy', [ClassName]);
end;

function Cone_TrianglesCount(State: TX3DGraphTraverseState;
  const Sides, Bottom: boolean; const Slices, Stacks: Cardinal): Cardinal;
begin
  Result := 0;
  if Sides then
    Result := Result + (Slices * ((Stacks - 1) * 2 + 1));
  if Bottom then
    Result := Result + Slices;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TConeNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Cone_TrianglesCount(State,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM],
    DefaultTriangulationSlices, DefaultTriangulationStacks);
end;
{$warnings on}

function TConeNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Cone_TrianglesCount(State,
    FdSide.Value, FdBottom.Value, CalculateSlices, CalculateStacks);
end;

function Box_TrianglesCount(State: TX3DGraphTraverseState): Cardinal;
begin
  result := 6 * 2;
end;

function TCubeNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Box_TrianglesCount(State);
end;

function TBoxNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Box_TrianglesCount(State);
end;

function Cylinder_TrianglesCount(State: TX3DGraphTraverseState;
  const Bottom, Side, Top: boolean; const Slices, Stacks: Cardinal): Cardinal;
begin
  Result := 0;
  if Side then
    Result := Result + (2 * Slices * Stacks);
  if Top then
    Result := Result + Slices;
  if Bottom then
    Result := Result + Slices;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TCylinderNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP],
    DefaultTriangulationSlices, 1 { no stacks used for cylinder });
end;
{$warnings on}

function TCylinderNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State,
    FdBottom.Value, FdSide.Value, FdTop.Value, CalculateSlices, CalculateStacks);
end;

function IndexedPolygons_TrianglesCount(
  State: TX3DGraphTraverseState;
  CoordIndex: TMFLong; Coord: TMFVec3f): Cardinal;
var
  BeginIndex, EndIndex: integer;
begin
  if Coord = nil then
    Exit(0);

  BeginIndex := 0;
  result := 0;
  while BeginIndex < CoordIndex.Count do
  begin
    EndIndex := BeginIndex;
    while (EndIndex < CoordIndex.Count) and
          (CoordIndex.Items.L[EndIndex] <> -1) do
      Inc(EndIndex);

    result := result + (Cardinal(Max(EndIndex - BeginIndex - 2, 0)));
    BeginIndex := EndIndex + 1;
  end;
end;

function TIndexedFacesOrTrianglesNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State,
    FdCoordIndex, InternalCoordinates(State));
end;

function TIndexedFaceSetNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State,
    FdCoordIndex, InternalCoordinates(State));
end;

function TIndexedLineSetNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TIndexedLineSetNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TLineSetNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TPointSetNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TPointSetNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function Sphere_TrianglesCount(State: TX3DGraphTraverseState;
  const Slices, Stacks: Cardinal): Cardinal;
begin
  result := Slices * ((Stacks - 2) * 2 + 2);
end;

function TSphereNode_1.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Sphere_TrianglesCount(State,
    DefaultTriangulationSlices, DefaultTriangulationStacks);
end;

function TSphereNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := Sphere_TrianglesCount(State,
    CalculateSlices, CalculateStacks);
end;

{ TElevationGridNode --------------------------------------------------------- }

function TElevationGridNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if IsNotEmpty then
    Result := (FdXDimension.Value - 1) * (FdZDimension.Value - 1) * 2 else
    Result := 0;
end;

function TElevationGridNode.VerticesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if IsNotEmpty then
    Result := FdXDimension.Value * FdZDimension.Value else
    Result := 0;
end;

{ TExtrusionNode ------------------------------------------------------------- }

function TExtrusionNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
var
  E: TVRMLExtrusion;
begin
  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    Result := (E.High + 1) * (FdCrossSection.Count - 1) * 2;
    if FdCrossSection.Count >= 2 then
    begin
      if FdBeginCap.Value then
        Result := Result + (Max(FdCrossSection.Count - 2 - Integer(E.CrossSectionOmit), 0));
      if FdEndCap.Value then
        Result := Result + (Max(FdCrossSection.Count - 2 - Integer(E.CrossSectionOmit), 0));
    end;
  finally FreeAndNil(E) end;
end;

{ X3D [Indexed] Triangle/Quad Set/Strip/Fan ---------------------------------- }

function TIndexedTriangleSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if InternalCoordinates(State) <> nil then
    Result := CoordIndexField.Count div 3 else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TTriangleSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := InternalCoordinates(State);
  if ACoord <> nil then
    Result := ACoord.Count div 3 else
    Result := 0;
end;

function TIndexedTriangleFanSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State,
    FdIndex, InternalCoordinates(State));
end;

function TTriangleFanSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if InternalCoordinates(State) <> nil then
    { For each fan, add it's count - 2. }
    Result := FdFanCount.Items.Sum - FdFanCount.Items.Count * 2 else
    Result := 0;
end;

function TIndexedTriangleStripSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State,
    FdIndex, InternalCoordinates(State));
end;

function TTriangleStripSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if InternalCoordinates(State) <> nil then
    { For each strip, add it's count - 2. }
    Result := FdStripCount.Items.Sum - FdStripCount.Items.Count * 2 else
    Result := 0;
end;

function TIndexedQuadSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  if InternalCoordinates(State) <> nil then
    Result := 2 * (FdIndex.Count div 4) else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TQuadSetNode.TrianglesCount(
  State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := InternalCoordinates(State);
  if ACoord <> nil then
    Result := 2 * (ACoord.Count div 4) else
    Result := 0;
end;

{ ---------------------------------------------------------------------------- }
