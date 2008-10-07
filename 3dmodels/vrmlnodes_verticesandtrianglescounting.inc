{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ implementacja metod VerticesCount i TrianglesCount dla specyficznych
  potomkow TVRMLGeometryNode }

function TVRMLGeometryNode.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  C: TMFVec3f;
  P: TVRMLGeometryNode;
begin
  if Coord(State, C) then
  begin
    if C <> nil then
      Result := C.Count else
      Result := 0;
  end else
  begin
    P := Proxy;
    if P <> nil then
    begin
      try
        Result := P.VerticesCount(State, OverTriangulate);
      finally FreeAndNil(P) end;
    end else
      raise EInternalError.Create('TVRMLGeometryNode.VerticesCount not overridden, and node not coordinate-based and without a Proxy');
  end;
end;

function TVRMLGeometryNode.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  P: TVRMLGeometryNode;
begin
  P := Proxy;
  if P <> nil then
  begin
    try
      Result := P.TrianglesCount(State, OverTriangulate);
    finally FreeAndNil(P) end;
  end else
    raise EInternalError.Create('TVRMLGeometryNode.TrianglesCount not overridden, and node without a Proxy');
end;

{ just a shortcuts for names often used in this file }
{$define QUADRIC_SLICES:=(State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS:=(State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS:=(State.LastNodes.KambiTriangulation.RectDivisions)}

function TNodeAsciiText_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin result := 0;{TODO} end;

function TNodeAsciiText_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin result := 0;{TODO} end;

function TNodeText.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin result := 0;{TODO} end;

function TNodeText.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin result := 0;{TODO} end;

function TNodeText3D.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin result := 0;{TODO} end;

function TNodeText3D.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
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

function TNodeCone_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cone_TrianglesCount(State, OverTriangulate,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

function TNodeCone_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cone_TrianglesCount(State, OverTriangulate,
    FdSide.Value, FdBottom.Value);
end;

function Cone_VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  const Sides, Bottom: boolean): Cardinal;
begin
 if OverTriangulate then
 begin
  if Bottom and Sides then
   result := QUADRIC_SLICES + 1 + (QUADRIC_STACKS-1)*QUADRIC_SLICES + 1 else
  if Bottom then
   result := QUADRIC_SLICES + 1 else
  if Sides then
   result := QUADRIC_SLICES + (QUADRIC_STACKS-1)*QUADRIC_SLICES + 1 else
   result := 0;
 end else
 begin
  if Bottom or Sides then
  begin
   result := QUADRIC_SLICES;
   if Bottom then Inc(result);
   if Sides then Inc(result);
  end else
   result := 0;
 end;
end;

function TNodeCone_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cone_VerticesCount(State, OverTriangulate,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

function TNodeCone_2.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cone_VerticesCount(State, OverTriangulate,
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

function TNodeCube_1.TrianglesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  Result := Box_TrianglesCount(State, OverTriangulate);
end;

function TNodeBox.TrianglesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  Result := Box_TrianglesCount(State, OverTriangulate);
end;

function Box_VerticesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
    {6 scian, w srodku kazdej Sqr(RECT_DIVISIONS) vertexow,
     12 krawedzi, na srodku kazdej RECT_DIVISIONS vertexow,
     8 rogow }
    result := 6 * Sqr(RECT_DIVISIONS) + 12*RECT_DIVISIONS + 8 else
    result := 8;
end;

function TNodeCube_1.VerticesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  Result := Box_VerticesCount(State, OverTriangulate);
end;

function TNodeBox.VerticesCount(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean): Cardinal;
begin
  Result := Box_VerticesCount(State, OverTriangulate);
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

function TNodeCylinder_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State, OverTriangulate,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

function TNodeCylinder_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cylinder_TrianglesCount(State, OverTriangulate,
    FdBottom.Value, FdSide.Value, FdTop.Value);
end;

function Cylinder_VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  const Bottom, Side, Top: boolean): Cardinal;
begin
 result := 0;
 if Side then
 begin
  if OverTriangulate then
   result += (QUADRIC_STACKS+1) * QUADRIC_SLICES else
   result += 2*QUADRIC_SLICES;
  if Top then Inc(result);
  if Bottom then Inc(result);
 end else
 begin
  if Top then result += QUADRIC_SLICES + 1;
  if Bottom then result += QUADRIC_SLICES + 1;
 end;
end;

function TNodeCylinder_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cylinder_VerticesCount(State, OverTriangulate,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

function TNodeCylinder_2.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Cylinder_VerticesCount(State, OverTriangulate,
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
          (CoordIndex.Items.Items[EndIndex] <> -1) do
      Inc(EndIndex);

    result += Cardinal(Max(EndIndex - BeginIndex - 2, 0));
    BeginIndex := EndIndex + 1;
  end;
end;

function TNodeIndexed_Faces_Or_Triangles_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdCoordIndex, Coordinates(State));
end;

function TNodeIndexedFaceSet_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdCoordIndex, Coordinates(State));
end;

function TNodeIndexedLineSet_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := 0;
end;

function TNodeIndexedLineSet_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := 0;
end;

function TNodeLineSet.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := 0;
end;

function TNodePointSet_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := 0;
end;

function TNodePointSet_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := 0;
end;

function TNodePointSet_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var dummy, numPoints: integer;
begin
 CalculateRange(State.LastNodes.Coordinate3, dummy, numPoints);
 result := numPoints;
end;

function Sphere_TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := QUADRIC_SLICES * ((QUADRIC_STACKS-2)*2 + 2);
end;

function TNodeSphere_1.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Sphere_TrianglesCount(State, OverTriangulate);
end;

function TNodeSphere_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Sphere_TrianglesCount(State, OverTriangulate);
end;

function Sphere_VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  result := QUADRIC_SLICES * (QUADRIC_STACKS-1) + 2;
end;

function TNodeSphere_1.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Sphere_VerticesCount(State, OverTriangulate);
end;

function TNodeSphere_2.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := Sphere_VerticesCount(State, OverTriangulate);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ TNodeElevationGrid --------------------------------------------------------- }

function TNodeElevationGrid.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if IsNotEmpty then
    Result := (FdXDimension.Value - 1) * (FdZDimension.Value - 1) * 2 else
    Result := 0;
end;

function TNodeElevationGrid.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if IsNotEmpty then
    Result := FdXDimension.Value * FdZDimension.Value else
    Result := 0;
end;

{ TNodeExtrusion ------------------------------------------------------------- }

function TNodeExtrusion.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  E: TVRMLExtrusion;
begin
  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    Result := (E.High + 1) * FdCrossSection.Count;
  finally FreeAndNil(E) end;
end;

function TNodeExtrusion.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
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

function TNodeIndexedTriangleSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if Coordinates(State) <> nil then
    Result := CoordIndex.Count div 3 else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TNodeTriangleSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := Coordinates(State);
  if ACoord <> nil then
    Result := ACoord.Count div 3 else
    Result := 0;
end;

function TNodeIndexedTriangleFanSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdIndex, Coordinates(State));
end;

function TNodeTriangleFanSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if Coordinates(State) <> nil then
    { For each fan, add it's count - 2. }
    Result := FdFanCount.Items.Sum - FdFanCount.Items.Count * 2 else
    Result := 0;
end;

function TNodeIndexedTriangleStripSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := IndexedPolygons_TrianglesCount(State, OverTriangulate,
    FdIndex, Coordinates(State));
end;

function TNodeTriangleStripSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if Coordinates(State) <> nil then
    { For each strip, add it's count - 2. }
    Result := FdStripCount.Items.Sum - FdStripCount.Items.Count * 2 else
    Result := 0;
end;

function TNodeIndexedQuadSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  if Coordinates(State) <> nil then
    Result := 2 * (FdIndex.Count div 4) else
    { If Coord is non-nil but has not enough items as indexed by
      CoordIndex, it's invalid according to X3D spec.
      So we don't worry about this case. }
    Result := 0;
end;

function TNodeQuadSet.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  ACoord: TMFVec3f;
begin
  ACoord := Coordinates(State);
  if ACoord <> nil then
    Result := 2 * (ACoord.Count div 4) else
    Result := 0;
end;

{ ---------------------------------------------------------------------------- }

function TNodeRectangle2D.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := 2;
end;

function TNodeRectangle2D.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := 4;
end;

function TNodeCircle2D.TrianglesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := 0; { no triangles }
end;

function TNodeCircle2D.VerticesCount(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := State.LastNodes.KambiTriangulation.QuadricSlices;
end;
