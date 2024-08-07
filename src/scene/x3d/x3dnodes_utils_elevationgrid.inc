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
    if PointsDistanceSqr(Coords.L[I1], Coords.L[I3]) <
       PointsDistanceSqr(Coords.L[I2], Coords.L[I4]) then
    begin
      CoordIndexes.L[IndexNum    ] := I1;
      CoordIndexes.L[IndexNum + 1] := I2;
      CoordIndexes.L[IndexNum + 2] := I3;
      CoordIndexes.L[IndexNum + 3] := I3;
      CoordIndexes.L[IndexNum + 4] := I4;
      CoordIndexes.L[IndexNum + 5] := I1;
    end else
    begin
      CoordIndexes.L[IndexNum    ] := I1;
      CoordIndexes.L[IndexNum + 1] := I2;
      CoordIndexes.L[IndexNum + 2] := I4;
      CoordIndexes.L[IndexNum + 3] := I2;
      CoordIndexes.L[IndexNum + 4] := I3;
      CoordIndexes.L[IndexNum + 5] := I4;
    end;
  end;

var
  TriangleSet: TIndexedTriangleSetNode absolute Result;
  XDim, ZDim: Integer; {< shortcuts for appropriate fields values }
  TexCoordProvided: boolean;
  I, J, NextIndex: Integer;
  CoordNode: TCoordinateNode;
  TexCoords: TVector2List;
begin
  TriangleSet := TIndexedTriangleSetNode.Create(X3DName, BaseUrl);
  try
    XDim := FdXDimension.Value;
    ZDim := FdZDimension.Value;

    if IsNotEmpty then
    begin
      CoordNode := TCoordinateNode.Create('', BaseUrl);
      TriangleSet.Coord := CoordNode;
      Coords := CoordNode.FdPoint.Items;

      { calculate TexCoordProvided, set TriangleSet.FdTexCoord, maybe set TexCoords }
      TexCoordProvided :=
        (FdTexCoord.Value <> nil) and
        (FdTexCoord.Value is TTextureCoordinateNode) and
        (TTextureCoordinateNode(FdTexCoord.Value).FdPoint.Count >=
          XDim * ZDim);
      if TexCoordProvided then
      begin
        TriangleSet.FdTexCoord.Value := FdTexCoord.Value;
        TexCoords := nil; // just silence Delphi warning
      end else
      begin
        TriangleSet.FdTexCoord.Value := TTextureCoordinateNode.Create('', BaseUrl);
        TexCoords := TTextureCoordinateNode(TriangleSet.FdTexCoord.Value).FdPoint.Items;
      end;

      { generate coords (and other per-vertex stuff: tex coords) }
      Coords.Count := XDim * ZDim;
      if not TexCoordProvided then
        TexCoords.Count := XDim * ZDim;
      for J := 0 to ZDim - 1 do
        for I := 0 to XDim - 1 do
        begin
          Coords.L[I + J * XDim] := Vector3(
            FdXSpacing.Value * I,
            FdHeight.Items.L[I + J * XDim],
            FdZSpacing.Value * J);

          if not TexCoordProvided then
            TexCoords.L[I + J * XDim] := Vector2(
              I / (XDim - 1),
              J / (ZDim - 1));
        end;

      { generate quads indexes }
      CoordIndexes := TriangleSet.CoordIndexField.Items;
      CoordIndexes.Count := (XDim - 1) * (ZDim - 1) * 6;
      NextIndex := 0;
      for J := 1 to ZDim - 1 do
        for I := 1 to XDim - 1 do
        begin
          { Vertices are ordered such that face is CCW from up
            (i.e. looking from positive Y axis). }
          Quad(NextIndex,
               I     + (J - 1) * XDim,
               I - 1 + (J - 1) * XDim,
               I - 1 +  J      * XDim,
               I     +  J      * XDim);
          NextIndex := NextIndex + 6;
        end;
      Assert(NextIndex = CoordIndexes.Count);
    end;

    TriangleSet.Solid := Solid;
    TriangleSet.Ccw := Ccw;
    { We order our coords such that we can simply copy normal/color nodes }
    TriangleSet.NormalPerVertex := NormalPerVertex;
    TriangleSet.Normal := Normal;
    TriangleSet.ColorPerVertex := ColorPerVertex;
    TriangleSet.Color := Color;
    // Not available: TriangleSet.CreaseAngle := CreaseAngle;
    {$ifndef CASTLE_SLIM_NODES}
    TriangleSet.FogCoord := FogCoord;
    TriangleSet.FdAttrib.AssignValue(FdAttrib);
    {$endif}
  except FreeAndNil(Result); raise end;
end;

