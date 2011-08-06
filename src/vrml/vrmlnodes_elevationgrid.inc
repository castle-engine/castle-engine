{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function TNodeElevationGrid.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;

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
  Coords: TDynVector3SingleArray;
  CoordIndexes: TDynLongIntArray;

  { Add to CoordIndexes a quad from given indexes.
    Actually we add two triangles, looking at Coord to choose the best
    triangulation. }
  procedure Quad(const IndexNum, I1, I2, I3, I4: Integer);
  begin
    if PointsDistanceSqr(Coords.List^[I1], Coords.List^[I3]) <
       PointsDistanceSqr(Coords.List^[I2], Coords.List^[I4]) then
    begin
      CoordIndexes.List^[IndexNum    ] := I1;
      CoordIndexes.List^[IndexNum + 1] := I2;
      CoordIndexes.List^[IndexNum + 2] := I3;
      CoordIndexes.List^[IndexNum + 3] := -1;
      CoordIndexes.List^[IndexNum + 4] := I3;
      CoordIndexes.List^[IndexNum + 5] := I4;
      CoordIndexes.List^[IndexNum + 6] := I1;
      CoordIndexes.List^[IndexNum + 7] := -1;
    end else
    begin
      CoordIndexes.List^[IndexNum    ] := I1;
      CoordIndexes.List^[IndexNum + 1] := I2;
      CoordIndexes.List^[IndexNum + 2] := I4;
      CoordIndexes.List^[IndexNum + 3] := -1;
      CoordIndexes.List^[IndexNum + 4] := I2;
      CoordIndexes.List^[IndexNum + 5] := I3;
      CoordIndexes.List^[IndexNum + 6] := I4;
      CoordIndexes.List^[IndexNum + 7] := -1;
    end;
  end;

var
  IFS: TNodeIndexedFaceSet absolute Result;
  XDimension, ZDimension: Integer; {< shortcuts for appropriate fields values }
  TexCoordProvided: boolean;
  I, J, NextIndex: Integer;
  CoordNode: TNodeCoordinate;
  TexCoords: TDynVector2SingleArray;
begin
  IFS := TNodeIndexedFaceSet.Create(NodeName, WWWBasePath);
  try
    XDimension := FdXDimension.Value;
    ZDimension := FdZDimension.Value;

    if IsNotEmpty then
    begin
      CoordNode := TNodeCoordinate.Create('', WWWBasePath);
      IFS.FdCoord.Value := CoordNode;
      Coords := CoordNode.FdPoint.Items;

      { calculate TexCoordProvided, set IFS.FdTexCoord, maybe set TexCoords }
      TexCoordProvided :=
        (FdTexCoord.Value <> nil) and
        (FdTexCoord.Value is TNodeTextureCoordinate) and
        (TNodeTextureCoordinate(FdTexCoord.Value).FdPoint.Count >=
          XDimension * ZDimension);
      if TexCoordProvided then
        IFS.FdTexCoord.Value := FdTexCoord.Value else
      begin
        IFS.FdTexCoord.Value := TNodeTextureCoordinate.Create('', WWWBasePath);
        TexCoords := TNodeTextureCoordinate(IFS.FdTexCoord.Value).FdPoint.Items;
      end;

      { generate coords (and other per-vertex stuff: tex coords) }
      Coords.Count := XDimension * ZDimension;
      if not TexCoordProvided then
        TexCoords.Count := XDimension * ZDimension;
      for J := 0 to ZDimension - 1 do
        for I := 0 to XDimension - 1 do
        begin
          Coords.List^[I + J * XDimension] := Vector3Single(
            FdXSpacing.Value * I,
            FdHeight.Items.List^[I + J * XDimension],
            FdZSpacing.Value * J);

          if not TexCoordProvided then
            TexCoords.List^[I + J * XDimension] := Vector2Single(
              I / (XDimension - 1),
              J / (ZDimension - 1));
        end;

      { generate quads indexes }
      CoordIndexes := IFS.CoordIndex.Items;
      CoordIndexes.Count := (XDimension - 1) * (ZDimension - 1) * 8;
      NextIndex := 0;
      for J := 1 to ZDimension - 1 do
        for I := 1 to XDimension - 1 do
        begin
          { Vertices are ordered such that face is CCW from up
            (i.e. looking from positive Y axis). }
          Quad(NextIndex,
               I     + (J - 1) * XDimension,
               I - 1 + (J - 1) * XDimension,
               I - 1 +  J      * XDimension,
               I     +  J      * XDimension);
          NextIndex += 8;
        end;
      Assert(NextIndex = CoordIndexes.Count);
    end;

    IFS.FdSolid.Value := FdSolid.Value;
    IFS.FdCcw.Value := FdCcw.Value;
    { We order our coords such that we can simply copy normal/color nodes }
    IFS.FdNormalPerVertex.Value := FdNormalPerVertex.Value;
    IFS.FdNormal.Value := FdNormal.Value;
    IFS.FdColorPerVertex.Value := FdColorPerVertex.Value;
    IFS.FdColor.Value := FdColor.Value;
    IFS.FdCreaseAngle.Value := FdCreaseAngle.Value;
    IFS.FdFogCoord.Value := FdFogCoord.Value;
    IFS.FdAttrib.AssignValue(FdAttrib);
  except FreeAndNil(Result); raise end;
end;

function TNodeElevationGrid.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;
