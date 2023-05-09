{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function TElevationGridNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;

{ I initially rendered ElevationGrid directly, by GL_QUAD_STRIP
  for each row. Fast, but has troubles:

  - When colorPerVertex is different than normalPerVertex,
    neither flat nor smooth shading is good.
  - Normal generation was a pain. A code generating smooth-only normals
    was implemented, and was already a pain to maintain (since with IndexedFaceSet,
    we already had normal generation implemented).
    creaseAngle was not supported, only all smooth or all flat was supported.
  - Rendering by quads was sometimes not fully correct, as elevation grid
    quads were not really planar. So with GPU performing any triangulation
    it wants, results are undefined and not always the best.
    Right now we triangulate ourselves along the shortest diagonal,
    which improves the look much.
    IndexedFaceSet is rendered through GL_TRIANGLES anyway.

  Using proxy to render ElevationGrid solves above troubles.
  Using proxy also makes simpler implementation than rendering,
  and also it's used for triangulating.
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
      CoordIndexes.L[IndexNum + 3] := -1;
      CoordIndexes.L[IndexNum + 4] := I3;
      CoordIndexes.L[IndexNum + 5] := I4;
      CoordIndexes.L[IndexNum + 6] := I1;
      CoordIndexes.L[IndexNum + 7] := -1;
    end else
    begin
      CoordIndexes.L[IndexNum    ] := I1;
      CoordIndexes.L[IndexNum + 1] := I2;
      CoordIndexes.L[IndexNum + 2] := I4;
      CoordIndexes.L[IndexNum + 3] := -1;
      CoordIndexes.L[IndexNum + 4] := I2;
      CoordIndexes.L[IndexNum + 5] := I3;
      CoordIndexes.L[IndexNum + 6] := I4;
      CoordIndexes.L[IndexNum + 7] := -1;
    end;
  end;

var
  IFS: TIndexedFaceSetNode absolute Result;
  XDim, ZDim: Integer; {< shortcuts for appropriate fields values }
  TexCoordProvided: boolean;
  I, J, NextIndex: Integer;
  CoordNode: TCoordinateNode;
  TexCoords: TVector2List;
begin
  IFS := TIndexedFaceSetNode.Create(X3DName, BaseUrl);
  try
    XDim := FdXDimension.Value;
    ZDim := FdZDimension.Value;

    if IsNotEmpty then
    begin
      CoordNode := TCoordinateNode.Create('', BaseUrl);
      IFS.Coord := CoordNode;
      Coords := CoordNode.FdPoint.Items;

      { calculate TexCoordProvided, set IFS.FdTexCoord, maybe set TexCoords }
      TexCoordProvided :=
        (FdTexCoord.Value <> nil) and
        (FdTexCoord.Value is TTextureCoordinateNode) and
        (TTextureCoordinateNode(FdTexCoord.Value).FdPoint.Count >=
          XDim * ZDim);
      if TexCoordProvided then
      begin
        IFS.FdTexCoord.Value := FdTexCoord.Value;
        TexCoords := nil; // just silence Delphi warning
      end else
      begin
        IFS.FdTexCoord.Value := TTextureCoordinateNode.Create('', BaseUrl);
        TexCoords := TTextureCoordinateNode(IFS.FdTexCoord.Value).FdPoint.Items;
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
      CoordIndexes := IFS.CoordIndexField.Items;
      CoordIndexes.Count := (XDim - 1) * (ZDim - 1) * 8;
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
          NextIndex := NextIndex + 8;
        end;
      Assert(NextIndex = CoordIndexes.Count);
    end;

    IFS.Solid := Solid;
    IFS.Ccw := Ccw;
    { We order our coords such that we can simply copy normal/color nodes }
    IFS.NormalPerVertex := NormalPerVertex;
    IFS.Normal := Normal;
    IFS.ColorPerVertex := ColorPerVertex;
    IFS.Color := Color;
    IFS.CreaseAngle := CreaseAngle;
    {$ifndef CASTLE_SLIM_NODES}
    IFS.FogCoord := FogCoord;
    IFS.FdAttrib.AssignValue(FdAttrib);
    {$endif}
  except FreeAndNil(Result); raise end;
end;

