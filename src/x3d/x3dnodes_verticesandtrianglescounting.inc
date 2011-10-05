{
  Copyright 2003-2011 Michalis Kamburelis.

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
  OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := ProxyGeometry.VerticesCount(ProxyState, OverTriangulate, nil, nil);
end;

function TAbstractGeometryNode.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
var
  C: TMFVec3f;
begin
  if Coord(State, C) then
  begin
    if C <> nil then
      Result := C.Count else
      Result := 0;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyVerticesCount(OverTriangulate, ProxyGeometry, ProxyState) else
      raise EInternalError.CreateFmt('%s: TAbstractGeometryNode.VerticesCount not overridden, and node not coordinate-based and without a Proxy', [ClassName]);
  end;
end;

function TAbstractGeometryNode.CallProxyTrianglesCount(
  OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := ProxyGeometry.TrianglesCount(ProxyState, OverTriangulate, nil, nil);
end;

function TAbstractGeometryNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if ProxyGeometry <> nil then
    Result := CallProxyTrianglesCount(OverTriangulate, ProxyGeometry, ProxyState) else
    raise EInternalError.CreateFmt('%s: TAbstractGeometryNode.TrianglesCount not overridden, and node without a Proxy', [ClassName]);
end;

{ just a shortcuts for names often used in this file }
{$define QUADRIC_SLICES:=(State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS:=(State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS:=(State.LastNodes.KambiTriangulation.RectDivisions)}

function TAsciiTextNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function TAsciiTextNode_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function TTextNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function TTextNode.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function TText3DNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function TText3DNode.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin result := 0;{TODO} end;

function Cone_TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  const Sides, Bottom: boolean): Cardinal;
begin
 result := 0;
 if Sides then
 begin
  if OverTriangulate then
   result += QUADRIC_SLICES * ((QUADRIC_STACKS-1)*2 + 1) else
   result += QUADRIC_SLICES;
 end;
 if Bottom then
  result += QUADRIC_SLICES;
end;

function TConeNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Cone_TrianglesCount(State, OverTriangulate,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

function TConeNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Cone_TrianglesCount(State, OverTriangulate,
    FdSide.Value, FdBottom.Value);
end;

function Box_TrianglesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
    {6 scian, na kazdej Sqr(RECT_DIVISIONS+1) kwadracikow}
    result := Sqr(RECT_DIVISIONS+1)*2 *6 else
    result := 6 * 2;
end;

function TCubeNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Box_TrianglesCount(State, OverTriangulate);
end;

function TBoxNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Box_TrianglesCount(State, OverTriangulate);
end;

function Cylinder_TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  const Bottom, Side, Top: boolean): Cardinal;
begin
 result := 0;
 if Side then
 begin
  if OverTriangulate then
   result += 2 * QUADRIC_SLICES*QUADRIC_STACKS else
   result += 2 * QUADRIC_SLICES;
 end;
 if Top then result += QUADRIC_SLICES;
 if Bottom then result += QUADRIC_SLICES;
end;

function TCylinderNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State, OverTriangulate,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

function TCylinderNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State, OverTriangulate,
    FdBottom.Value, FdSide.Value, FdTop.Value);
end;

function IndexedPolygons_TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
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

    result += Cardinal(Max(EndIndex - BeginIndex - 2, 0));
    BeginIndex := EndIndex + 1;
  end;
end;

function TIndexedFacesOrTrianglesNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdCoordIndex, Coordinates(State));
end;

function TIndexedFaceSetNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdCoordIndex, Coordinates(State));
end;

function TIndexedLineSetNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TIndexedLineSetNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TLineSetNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TPointSetNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function TPointSetNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  result := 0;
end;

function Sphere_TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := QUADRIC_SLICES * ((QUADRIC_STACKS-2)*2 + 2);
end;

function TSphereNode_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Sphere_TrianglesCount(State, OverTriangulate);
end;

function TSphereNode.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := Sphere_TrianglesCount(State, OverTriangulate);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ TElevationGridNode --------------------------------------------------------- }

function TElevationGridNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if IsNotEmpty then
    Result := (FdXDimension.Value - 1) * (FdZDimension.Value - 1) * 2 else
    Result := 0;
end;

function TElevationGridNode.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if IsNotEmpty then
    Result := FdXDimension.Value * FdZDimension.Value else
    Result := 0;
end;

{ TExtrusionNode ------------------------------------------------------------- }

function TExtrusionNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
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
        Result += Cardinal(FdCrossSection.Count) - 2;
      if FdEndCap.Value then
        Result += Cardinal(FdCrossSection.Count) - 2;
    end;
  finally FreeAndNil(E) end;
end;

{ X3D [Indexed] Triangle/Quad Set/Strip/Fan ---------------------------------- }

function TIndexedTriangleSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if Coordinates(State) <> nil then
    Result := CoordIndex.Count div 3 else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TTriangleSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := Coordinates(State);
  if ACoord <> nil then
    Result := ACoord.Count div 3 else
    Result := 0;
end;

function TIndexedTriangleFanSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdIndex, Coordinates(State));
end;

function TTriangleFanSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if Coordinates(State) <> nil then
    { For each fan, add it's count - 2. }
    Result := FdFanCount.Items.Sum - FdFanCount.Items.Count * 2 else
    Result := 0;
end;

function TIndexedTriangleStripSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdIndex, Coordinates(State));
end;

function TTriangleStripSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if Coordinates(State) <> nil then
    { For each strip, add it's count - 2. }
    Result := FdStripCount.Items.Sum - FdStripCount.Items.Count * 2 else
    Result := 0;
end;

function TIndexedQuadSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  if Coordinates(State) <> nil then
    Result := 2 * (FdIndex.Count div 4) else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TQuadSetNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := Coordinates(State);
  if ACoord <> nil then
    Result := 2 * (ACoord.Count div 4) else
    Result := 0;
end;

{ ---------------------------------------------------------------------------- }
