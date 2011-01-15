{
  Copyright 2002-2010 Michalis Kamburelis.

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
  - Normal generation was a pain. A large code generating smooth-only normals
    was done.
  . creaseAngle was not supported,
    only all smooth or all flat was supported.

  Using proxy to render ElevationGrid solves above troubles.
  Using proxy also makes simpler implementation than rendering,
  and also it's used for triangulating.
  Although IndexedFaceSet is not so efficiently rendered
  (will be split into triangles).
}

var
  XDimension, ZDimension: Integer; {< shortcuts for appropriate fields values }
  TexCoordProvided: boolean;
  I, J, NextIndex: Integer;
  CoordNode: TNodeCoordinate;
  Coords: TDynVector3SingleArray;
  TexCoords: TDynVector2SingleArray;
  IFS: TNodeIndexedFaceSet_2 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
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
      for I := 0 to XDimension - 1 do
        for J := 0 to ZDimension - 1 do
        begin
          Coords.Items[I + J * XDimension] := Vector3Single(
            FdXSpacing.Value * I,
            FdHeight.Items.Items[I + J * XDimension],
            FdZSpacing.Value * J);

          if not TexCoordProvided then
            TexCoords.Items[I + J * XDimension] := Vector2Single(
              I / (XDimension - 1),
              J / (ZDimension - 1));
        end;

      { generate quads indexes }
      IFS.CoordIndex.Items.Count := (XDimension - 1) * (ZDimension - 1) * 5;
      NextIndex := 0;
      for I := 1 to XDimension - 1 do
        for J := 1 to ZDimension - 1 do
        begin
          { Vertices are ordered such that face is CCW from up
            (i.e. looking from positive Y axis). }
          IFS.CoordIndex.Items.Items[NextIndex] := I     + (J - 1) * XDimension; Inc(NextIndex);
          IFS.CoordIndex.Items.Items[NextIndex] := I - 1 + (J - 1) * XDimension; Inc(NextIndex);
          IFS.CoordIndex.Items.Items[NextIndex] := I - 1 +  J      * XDimension; Inc(NextIndex);
          IFS.CoordIndex.Items.Items[NextIndex] := I     +  J      * XDimension; Inc(NextIndex);
          IFS.CoordIndex.Items.Items[NextIndex] := -1;                           Inc(NextIndex);
        end;
      Assert(NextIndex = IFS.CoordIndex.Items.Count);
    end;

    IFS.FdSolid.Value := FdSolid.Value;
    IFS.FdCcw.Value := FdCcw.Value;
    { We order our coords such that we can simply copy normal/color nodes }
    IFS.FdNormalPerVertex.Value := FdNormalPerVertex.Value;
    IFS.FdNormal.Value := FdNormal.Value;
    IFS.FdColorPerVertex.Value := FdColorPerVertex.Value;
    IFS.FdColor.Value := FdColor.Value;
    IFS.FdCreaseAngle.Value := FdCreaseAngle.Value;
  except FreeAndNil(Result); raise end;
end;

function TNodeElevationGrid.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;
