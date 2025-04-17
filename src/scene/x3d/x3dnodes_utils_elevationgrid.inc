{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function TElevationGridNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  TriangleSet: TIndexedTriangleSetNode absolute Result;
begin
  TriangleSet := InternalCreateTriangles(
    FdXDimension.Value, FdZDimension.Value,
    FdXSpacing.Value, FdZSpacing.Value,
    FdHeight.Items,
    TexCoord,
    { Idea: We could expose TElevationGridNode.TriangulationIgnoreHeights
      as extra X3D field some day, if needed? }
    false
  );
  try
    // copy all additional fields, that are not handled by InternalCreateTriangles
    TriangleSet.X3DName := X3DName;
    TriangleSet.BaseUrl := BaseUrl;
    TriangleSet.Solid := Solid;
    TriangleSet.Ccw := Ccw;
    { We order our coords such that we can simply copy normal/color nodes }
    TriangleSet.NormalPerVertex := NormalPerVertex;
    // TODO: InternalCreateTriangles should take this, and generate own normals if none provided
    TriangleSet.Normal := Normal;
    TriangleSet.ColorPerVertex := ColorPerVertex;
    TriangleSet.Color := Color;
    // Not available: TriangleSet.CreaseAngle := CreaseAngle;
    {$ifndef CASTLE_SLIM_NODES}
    TriangleSet.FogCoord := FogCoord;
    TriangleSet.FdAttrib.AssignValue(FdAttrib);
    {$endif}
  except FreeAndNil(TriangleSet); raise end;
end;

class function TElevationGridNode.InternalCreateTriangles(
  const AXDimension, AZDimension: Integer;
  const AXSpacing, AZSpacing: Single;
  const AHeight: TSingleList;
  const ATexCoord: TAbstractTextureCoordinateNode;
  const TriangulationIgnoreHeights: Boolean): TIndexedTriangleSetNode;

{ How to render TElevationGridNode?

  - Quad strip? (direct GL_QUAD_STRIP or proxy on T[Indexed]QuadSetNode)?

    Not good:

    - When colorPerVertex is different than normalPerVertex,
      neither flat nor smooth shading is good.

    - With direct GL_QUAD_STRIP, normal generation was a pain.
      We want to instead use proxy on something that already implements
      normal support, honoring creaseAngle.

    - Rendering by quads was sometimes not fully correct, as elevation grid
      quads were not really planar. So with GPU performing any triangulation
      it wants, results are undefined and not always the best.

  - By TIndexedFaceSetNode proxy?

    Mostly good, but it's not as straightforward to render by renderer
    as TIndexedTriangleSetNode. It needs to be converted to triangles.

  - By TIndexedTriangleSetNode proxy?

    Better than TIndexedFaceSetNode for renderer.

    We lose support for creaseAngle, but it's not a big deal.
    Our TCastleTerrainData wasn't using it anyway, always setting creaseAngle to 4.
}
var
  Coords: TVector3List;
  CoordIndexes: TInt32List;

  { Add to CoordIndexes a quad from given indexes.
    Actually we add two triangles, looking at Coord to choose the best
    triangulation. }
  procedure Quad(const IndexNum, I1, I2, I3, I4: Integer);
  begin
    if TriangulationIgnoreHeights or
       ( PointsDistanceSqr(Coords.L[I1], Coords.L[I3]) >
         PointsDistanceSqr(Coords.L[I2], Coords.L[I4]) ) then
    begin
      CoordIndexes.L[IndexNum    ] := I1;
      CoordIndexes.L[IndexNum + 1] := I2;
      CoordIndexes.L[IndexNum + 2] := I4;
      CoordIndexes.L[IndexNum + 3] := I2;
      CoordIndexes.L[IndexNum + 4] := I3;
      CoordIndexes.L[IndexNum + 5] := I4;
    end else
    begin
      CoordIndexes.L[IndexNum    ] := I1;
      CoordIndexes.L[IndexNum + 1] := I2;
      CoordIndexes.L[IndexNum + 2] := I3;
      CoordIndexes.L[IndexNum + 3] := I3;
      CoordIndexes.L[IndexNum + 4] := I4;
      CoordIndexes.L[IndexNum + 5] := I1;
    end;
  end;

var
  I, J, NextIndex: Integer;
  CoordNode: TCoordinateNode;
  TexCoordsToGenerate: TVector2List;
begin
  Result := TIndexedTriangleSetNode.Create;
  try
    if ClassIsNotEmpty(
         AXDimension, AZDimension,
         AXSpacing, AZSpacing, AHeight) then
    begin
      CoordNode := TCoordinateNode.Create;
      Result.Coord := CoordNode;
      Coords := CoordNode.FdPoint.Items;

      { calculate TexCoordProvided, set Result.FdTexCoord, maybe set TexCoords }
      if ATexCoord <> nil then
      begin
        Result.FdTexCoord.Value := ATexCoord;
        TexCoordsToGenerate := nil;
      end else
      begin
        Result.FdTexCoord.Value := TTextureCoordinateNode.Create;
        TexCoordsToGenerate := TTextureCoordinateNode(Result.FdTexCoord.Value).
          FdPoint.Items;
      end;

      { generate coords (and other per-vertex stuff: tex coords) }
      Coords.Count := AXDimension * AZDimension;
      if TexCoordsToGenerate <> nil then
        TexCoordsToGenerate.Count := AXDimension * AZDimension;
      for J := 0 to AZDimension - 1 do
        for I := 0 to AXDimension - 1 do
        begin
          Coords.L[I + J * AXDimension] := Vector3(
            AXSpacing * I,
            AHeight.L[I + J * AXDimension],
            AZSpacing * J);

          if TexCoordsToGenerate <> nil then
            TexCoordsToGenerate.L[I + J * AXDimension] := Vector2(
              I / (AXDimension - 1),
              J / (AZDimension - 1));
        end;

      { generate quads indexes }
      CoordIndexes := Result.CoordIndexField.Items;
      CoordIndexes.Count := (AXDimension - 1) * (AZDimension - 1) * 6;
      NextIndex := 0;
      for J := 1 to AZDimension - 1 do
        for I := 1 to AXDimension - 1 do
        begin
          { Vertices are ordered such that face is CCW from up
            (i.e. looking from positive Y axis). }
          Quad(NextIndex,
               I     + (J - 1) * AXDimension,
               I - 1 + (J - 1) * AXDimension,
               I - 1 +  J      * AXDimension,
               I     +  J      * AXDimension);
          NextIndex := NextIndex + 6;
        end;
      Assert(NextIndex = CoordIndexes.Count);
    end;

  except FreeAndNil(Result); raise end;
end;

