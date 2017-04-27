{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Make a sphere. }
procedure Sphere_Proxy(CoordIndex: TLongIntList;
  Coord: TVector3SingleList; Normal: TVector3SingleList;
  TexCoord: TVector2SingleList;
  OverTriangulate: boolean; const Radius: Single;
  const QuadricSlices, QuadricStacks: Cardinal);

  { We have rings numbered 0 ... QuadricStacks.
    The rings nr 0 and QuadricStacks have 0 radius (bottom and top of the sphere). }

  { For stack number calculate it's radius and height (in y). }
  procedure StackCircle(StackNum: integer; out StackRadius, Height: Single);
  var
    Alpha, S, C: Float;
  begin
    { Calculate correct StackRadius and Height for sphere of radius 1,
      and then scale by Radius. }

    { Divide the vertical ring into QuadricStacks equal pieces.
      This is much better idea than simply dividing along the Height,
      because it allows the top/bottom of the sphere to look much better.
      This is also what OpenGL quadrics do.
      You can compare by using below

        StackRadius := Sqrt( 1 - Sqr(1 - 2*StackNum/QuadricStacks) ) * Radius;
        Height := (-1 + StackNum * 2/QuadricStacks) * Radius;
    }
    Alpha := Pi * StackNum / QuadricStacks;
    SinCos(Alpha, S, C);
    StackRadius :=  S * Radius;
    Height      := -C * Radius;
  end;

var
  StackRadius, StackHeight: Single;
  StackFaceIndex, BottomIndex, TopIndex, BottomCircleIndex, CircleIndex,
    StackNum, I: Integer;
begin
  { For VRML 1.0, some of these MF fields by default have non-empty content.
    It's safest to just clean them. }
  CoordIndex.Count := 0;
  Coord.Count := 0;
  Normal.Count := 0;
  if TexCoord <> nil then TexCoord.Count := 0;

  { We generate QuadricSlices + 1 points, not just QuadricSlices for each stack.
    Reason: texture coordinates must be different at the last point
    (1.0) than the first (0.0).
    Otherwise texture seam would not be correctly closed. }

  { Even for the bottom/top point, we create many points, because
    they need different tex coords. }
  BottomIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices do
  begin
    Coord.L[BottomIndex + I] := Vector3Single(0, -Radius, 0);
    Normal.L[BottomIndex + I] := Vector3Single(0, -1, 0);
  end;
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices do
      TexCoord.L[BottomIndex + I] := Vector2Single(I / QuadricSlices, 0);
  end;

  BottomCircleIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  StackCircle(1, StackRadius, StackHeight);
  GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(BottomCircleIndex));
  Coord[Coord.Count - 1] := Coord.L[BottomCircleIndex];
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices - 1 do
    Normal.L[BottomCircleIndex + I] := Normalized(Coord.L[BottomCircleIndex + I]);
  Normal[Normal.Count - 1] := Normal.L[BottomCircleIndex];
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices  do
      TexCoord.L[BottomCircleIndex + I] := Vector2Single(I / QuadricSlices, 1 / QuadricStacks);
  end;

  { Bottom stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    CoordIndex.L[StackFaceIndex + I * 4    ] := BottomCircleIndex + I + 1;
    CoordIndex.L[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 2] := BottomIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 3] := -1;
  end;

  for StackNum := 2 to QuadricStacks - 1 do
  begin
    StackCircle(StackNum, StackRadius, StackHeight);

    CircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices + 1;
    StackCircle(StackNum, StackRadius, StackHeight);
    GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(CircleIndex));
    Coord[Coord.Count - 1] := Coord.L[CircleIndex];
    Normal.Count := Normal.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[CircleIndex + I] := Normalized(Coord.L[CircleIndex + I]);
    Normal[Normal.Count - 1] := Normal.L[CircleIndex];
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
      for I := 0 to QuadricSlices  do
        TexCoord.L[CircleIndex + I] := Vector2Single(I / QuadricSlices, StackNum / QuadricStacks);
    end;

    { Next stack, from quads }
    StackFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
    for I := 0 to QuadricSlices - 1 do
    begin
      CoordIndex.L[StackFaceIndex + I * 5    ] := BottomCircleIndex + I;
      CoordIndex.L[StackFaceIndex + I * 5 + 1] := BottomCircleIndex + I + 1;
      CoordIndex.L[StackFaceIndex + I * 5 + 2] := CircleIndex + I + 1;
      CoordIndex.L[StackFaceIndex + I * 5 + 3] := CircleIndex + I;
      CoordIndex.L[StackFaceIndex + I * 5 + 4] := -1;
    end;

    BottomCircleIndex := CircleIndex;
  end;

  TopIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices do
  begin
    Coord.L[TopIndex + I] := Vector3Single(0, Radius, 0);
    Normal.L[TopIndex + I] := Vector3Single(0, 1, 0);
  end;
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices do
      TexCoord.L[TopIndex + I] := Vector2Single(I / QuadricSlices, 1);
  end;

  { Top stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    CoordIndex.L[StackFaceIndex + I * 4    ] := BottomCircleIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I + 1;
    CoordIndex.L[StackFaceIndex + I * 4 + 2] := TopIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 3] := -1;
  end;
end;

{ Make a wireframe sphere. }
procedure SphereWireframe_Proxy(CoordIndex: TLongIntList;
  Coord: TVector3SingleList; const Radius: Single;
  const QuadricSlices, QuadricStacks: Cardinal);

  { We have rings numbered 0 ... QuadricStacks.
    The rings nr 0 and QuadricStacks have 0 radius (bottom and top of the sphere). }

  { For stack number calculate it's radius and height (in y). }
  procedure StackCircle(StackNum: integer; out StackRadius, Height: Single);
  var
    Alpha, S, C: Float;
  begin
    { Calculate correct StackRadius and Height for sphere of radius 1,
      and then scale by Radius. }

    { Divide the vertical ring into QuadricStacks equal pieces.
      This is much better idea than simply dividing along the Height,
      because it allows the top/bottom of the sphere to look much better.
      This is also what OpenGL quadrics do.
      You can compare by using below

        StackRadius := Sqrt( 1 - Sqr(1 - 2*StackNum/QuadricStacks) ) * Radius;
        Height := (-1 + StackNum * 2/QuadricStacks) * Radius;
    }
    Alpha := Pi * StackNum / QuadricStacks;
    SinCos(Alpha, S, C);
    StackRadius :=  S * Radius;
    Height      := -C * Radius;
  end;

var
  StackRadius, StackHeight: Single;
  StackFaceIndex, BottomIndex, TopIndex, BottomCircleIndex, CircleIndex,
    StackNum, I, NextI: Integer;
begin
  CoordIndex.Count := 0;
  Coord.Count := 0;

  BottomIndex := Coord.Count;
  Coord.Count := Coord.Count + 1;
  Coord.L[BottomIndex] := Vector3Single(0, -Radius, 0);

  BottomCircleIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices;
  StackCircle(1, StackRadius, StackHeight);
  GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(BottomCircleIndex));

  { Bottom stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    NextI := (I + 1) mod QuadricSlices;
    CoordIndex.L[StackFaceIndex + I * 4    ] := BottomCircleIndex + NextI;
    CoordIndex.L[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 2] := BottomIndex;
    CoordIndex.L[StackFaceIndex + I * 4 + 3] := -1;
  end;

  for StackNum := 2 to QuadricStacks - 1 do
  begin
    StackCircle(StackNum, StackRadius, StackHeight);

    CircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices;
    StackCircle(StackNum, StackRadius, StackHeight);
    GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(CircleIndex));

    { Next stack, from quads (but we only need to add 3 lines) }
    StackFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
    for I := 0 to QuadricSlices - 1 do
    begin
      NextI := (I + 1) mod QuadricSlices;
      CoordIndex.L[StackFaceIndex + I * 5    ] := BottomCircleIndex + NextI;
      CoordIndex.L[StackFaceIndex + I * 5 + 1] := CircleIndex + NextI;
      CoordIndex.L[StackFaceIndex + I * 5 + 2] := CircleIndex + I;
      CoordIndex.L[StackFaceIndex + I * 5 + 3] := BottomCircleIndex + I;
      CoordIndex.L[StackFaceIndex + I * 5 + 4] := -1;
    end;

    BottomCircleIndex := CircleIndex;
  end;

  TopIndex := Coord.Count;
  Coord.Count := Coord.Count + 1;
  Coord.L[TopIndex] := Vector3Single(0, Radius, 0);

  { Top stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    NextI := (I + 1) mod QuadricSlices;
    CoordIndex.L[StackFaceIndex + I * 4    ] := BottomCircleIndex + I;
    CoordIndex.L[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + NextI;
    CoordIndex.L[StackFaceIndex + I * 4 + 2] := TopIndex;
    CoordIndex.L[StackFaceIndex + I * 4 + 3] := -1;
  end;
end;

function TSphereNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;

  procedure SphereWireframe;
  var
    CoordNode: TCoordinateNode;
    ILS: TIndexedLineSetNode absolute Result;
  begin
    ILS := TIndexedLineSetNode.Create(X3DName, BaseUrl);
    try
      CoordNode := TCoordinateNode.Create('', BaseUrl);
      ILS.FdCoord.Value := CoordNode;

      SphereWireframe_Proxy(ILS.FdCoordIndex.Items, CoordNode.FdPoint.Items,
        FdRadius.Value, CalculateSlices, CalculateStacks);
    except FreeAndNil(Result); raise end;
  end;

  procedure SphereFilled;
  var
    CoordNode: TCoordinateNode;
    NormalNode: TNormalNode;
    TexCoordNode: TTextureCoordinateNode;
    TexCoords: TVector2SingleList;
    IFS: TIndexedFaceSetNode absolute Result;
  begin
    IFS := TIndexedFaceSetNode.Create(X3DName, BaseUrl);
    try
      CoordNode := TCoordinateNode.Create('', BaseUrl);
      IFS.FdCoord.Value := CoordNode;

      NormalNode := TNormalNode.Create('', BaseUrl);
      IFS.FdNormal.Value := NormalNode;
      IFS.FdNormalPerVertex.Value := true;

      if (FdTexCoord.Value <> nil) and FdTexCoord.CurrentChildAllowed then
      begin
        { No need for CylinderCone_Proxy to create tex coords. }
        IFS.FdTexCoord.Value := FdTexCoord.Value;
        TexCoords := nil;
      end else
      begin
        TexCoordNode := TTextureCoordinateNode.Create('', BaseUrl);
        IFS.FdTexCoord.Value := TexCoordNode;
        TexCoords := TexCoordNode.FdPoint.Items;
      end;

      Sphere_Proxy(IFS.FdCoordIndex.Items,
        CoordNode.FdPoint.Items, NormalNode.FdVector.Items, TexCoords,
        OverTriangulate, FdRadius.Value,
        CalculateSlices, CalculateStacks);

      IFS.FdSolid.Value := FdSolid.Value;

      { Smooth everything. }
      IFS.FdCreaseAngle.Value := 4;
    except FreeAndNil(Result); raise end;
  end;

begin
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Shading = shWireframe) then
    SphereWireframe
  else
    SphereFilled;
end;

function TSphereNode_1.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
var
  CoordNode: TCoordinate3Node_1;
  NormalNode: TNormalNode;
  NormalBinding: TNormalBindingNode_1;
  TexCoordNode: TTextureCoordinate2Node_1;
  ShapeHints: TShapeHintsNode_1;
  MaterialBinding: TMaterialBindingNode_1;
  IFS: TIndexedFaceSetNode_1 absolute Result;
begin
  IFS := TIndexedFaceSetNode_1.Create(X3DName, BaseUrl);
  try
    { we have to modify State, so make a copy of it }
    State := TX3DGraphTraverseState.CreateCopy(State);

    CoordNode := TCoordinate3Node_1.Create('', BaseUrl);
    State.SetLastNodes(vsCoordinate3, CoordNode, true);

    NormalNode := TNormalNode.Create('', BaseUrl);
    State.SetLastNodes(vsNormal, NormalNode, true);

    NormalBinding := TNormalBindingNode_1.Create('', BaseUrl);
    { NormalBinding.value = PER_VERTEX means we use niPerVertexCoordIndexed,
      so coordIndex chooses the normal. }
    NormalBinding.FdValue.Value := BIND_PER_VERTEX;
    State.SetLastNodes(vsNormalBinding, NormalBinding, true);

    TexCoordNode := TTextureCoordinate2Node_1.Create('', BaseUrl);
    State.SetLastNodes(vsTextureCoordinate2, TexCoordNode, true);

    ShapeHints := TShapeHintsNode_1.Create('', BaseUrl);
    { For VRML 1.0, Sphere is never solid. }
    ShapeHints.FdshapeType.Value := SHTYPE_UNKNOWN;
    ShapeHints.FdvertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE;
    { Smooth everything. Not really needed, we use explicit normal node now. }
    ShapeHints.FdCreaseAngle.Value := 4;
    State.SetLastNodes(vsShapeHints, ShapeHints, true);

    { calculate MaterialBinding. Sphere has always only 1 material. }
    MaterialBinding := TMaterialBindingNode_1.Create('', BaseUrl);
    MaterialBinding.FdValue.Value := BIND_OVERALL;
    State.SetLastNodes(vsMaterialBinding, MaterialBinding, true);

    Sphere_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items,
      TexCoordNode.FdPoint.Items,
      OverTriangulate, FdRadius.Value,
      DefaultTriangulationSlices, DefaultTriangulationStacks);

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex).
      This is a problem of VRML 1.0 "state" idea --- there is no
      other way to "turn off" texture than to just use empty textureCoordIndex. }
    IFS.FdTextureCoordIndex.Items.Assign(IFS.FdCoordIndex.Items);
  except FreeAndNil(Result); raise end;
end;
