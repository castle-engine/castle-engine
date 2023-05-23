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

{ Utilities for making cone and cylinder shapes. }

{ Generate circle points.
  Circle is on const Y = Height, centered on XZ = (0, 0) with given Radius.
  We generate QuadricSlices points, writing them into Points array.
  Order of coords is along -Z, then -X, then +Z, then +X.

  When Radius = exactly 0, the points are guaranteed to be exactly equal.

  Assumes QuadricSlices >= 1. }
procedure GenerateCircle(const QuadricSlices: Cardinal;
  const Radius, Height: Single; Points: PVector3);
var
  AngleRad: Single;
  AngleSin, AngleCos: Float;
  I: integer;
begin
  if Radius <> 0 then
  begin
    Points^ := Vector3(0, Height, -Radius);
    Inc(Points);

    for I := 1 to QuadricSlices - 1 do
    begin
      AngleRad := (I / QuadricSlices) * 2 * Pi;
      SinCos(AngleRad, AngleSin, AngleCos);

      Points^ := Vector3(-AngleSin * Radius, Height, -AngleCos * Radius);
      Inc(Points);
    end;
  end else
  begin
    { The code above would work for this case too,
      but this is an optimized version. }
    for I := 0 to QuadricSlices - 1 do
    begin
      Points^.X := 0;
      Points^.Y := Height;
      Points^.Z := 0;
      Inc(Points);
    end;
  end;
end;

{ Generate QuadricSlices points on a 2D circle of radius 1.
  Shift things such that instead of [-1, 1] x [-1, 1],
  circle is inside [0, 1] x [0, 1].

  @param(InvertT Allows you to invert texture, suitable for top cap.) }
procedure GenerateTexCoordCircle(const QuadricSlices: Cardinal;
  Points: PVector2; const InvertT: boolean);
var
  AngleRad: Single;
  AngleSin, AngleCos: Float;
  I: integer;
begin
  if InvertT then
    Points^ := Vector2(0.5, 1) else
    Points^ := Vector2(0.5, 0);
  Inc(Points);

  for I := 1 to QuadricSlices - 1 do
  begin
    AngleRad := (I / QuadricSlices) * 2 * Pi;
    SinCos(AngleRad, AngleSin, AngleCos);

    AngleSin := -AngleSin;
    if not InvertT then AngleCos := -AngleCos;

    Points^ := Vector2((AngleSin + 1) / 2, (AngleCos + 1) / 2 );
    Inc(Points);
  end;
end;

{ Make a cylinder (with bottom and top radius
  potentially different) or a cone (which is indicated by TopRadius = 0).
  Generates values suitable e.g. for VRML/X3D IndexedFaceSet.
  This is a universal utility, for both VRML 1.0 and >= 2.0.

  Just like gluCylinder, it makes sense to implement cone and cylinder
  at once. Their topology is the same (yes, even at the cone top,
  see comments in the implementation).

  @param(TexCoord Texture coordinates to generate.
    Pass @nil if you don't need them.)

  @param(MaterialIndex VRML 1.0 material index, generated like for
    material binding per part. Will be used with
    material binding BIND_PER_FACE_INDEXED.
    We use PER_FACE_INDEXED instead of PER_VERTEX_INDEXED because
    PER_VERTEX_INDEXED isn't perfectly implemented (see OutsideBeginEnd
    notes), besides it matches our needs better.

    Pass @nil if you don't need it.)

  @param(TopRadius Radius of the upper cap.
    May be = 0, then Top does not matter (we'll never make
    a top cap in this case.)) }
procedure CylinderCone_Proxy(CoordIndex: TInt32List;
  Coord: TVector3List; Normal: TVector3List;
  TexCoord: TVector2List; MaterialIndex: TInt32List;
  const BottomRadius, TopRadius, Height: Single;
  const Bottom, Side, Top: boolean;
  const QuadricSlices, QuadricStacks: Cardinal);

{ How to make nice normals at the top of the cone?

    It's simply impossible to get nice smooth normals without adding additional
    stacks (that don't change the shape, so in a sense are wasteful).
    OpenGL gluCylinder also fails to make smooth cone normals
    when stacks = 1.

    Ideas:
    1. We can't use a single vertex (with one normal)
      at the top of the cone, obviously. This would smooth our normal
      vector between all side faces.

    2. First real idea for a fix was to use QuadricSlices * top points,
      and make each cone side face a triangle with different top vertex.
      However, this doesn't make cone side edges smooth: as each triangle
      has different normal vector at the top vertex, so normals are not
      matching on neighboring edges.

    3. Another idea was to cheat: represent each cone face as a quad.
      This means that each cone side face
      has 2 top vertexes, which may have different normal vectors.

      This is also what OpenGL GLU quadrics do --- you have to use gluCylinder
      with top radius 0 to get a cone. And at least SGI implementation
      (see http://cgit.freedesktop.org/mesa/mesa/tree/src/glu/sgi/libutil/quad.c)
      will actually use GL_QUAD_STRIP (just like for regular cylinder),
      and the top vertex will be simply duplicated on each face.

      For a moment I thought this will work, i.e. will make edges smooth.
      Normals match across neighboring edges, right?

      ... Except it doesn't. Probably because we simply split this
      quad into 2 triangles (during IndexedFaceSet rendering).
      And OpenGL quad rendering does the same, at least on chantal GPU.
      So one triangle is simply empty (rasterization doesn't produce anything),
      and the edges are still sharp.

      This *would* work if OpenGL would rasterize quads, interpolating
      across them correctly.

    After all, I use the 2. idea, duplicating top verts but not cheating
    with quads. Reasons:
    - since 3. doesn't work anyway, why waste memory
      (and bandwidth to OpenGL) for sending dummy triangles?
    - Besides, 3. makes TrianglesCount reported as more than the number
      of items inside the octree (because degenerate triangles are removed).
      While this is unavoidable in general (actual IndexedFaceSet may have
      degenerate triangles), there's no need to cause confusion in this case
      when we can avoid it.

  Why explicit normals?

    Initially, I thought that explicit normals are needed to set
    correct normals at the top of the cone. But, eventually,
    I found out that it's impossible to make them really smooth
    --- see the above discussion.

    The current reason: because we can. It's easy and already implemented.

  We use normal not NULL, with normalPerVertex = TRUE, and normalIndex empty.
  So coordIndex will be used to index normals.

  Why not use specific normalIndex?

    Using normalIndex would allow us to have shorter Coordinate array.
    For example top/bottom circle, and cone top vertexes, would not have
    to be duplicated in Coordinate array
    (just because it needs different normals on different faces).
    However, this is worthless optimization: for OpenGL vertex array rendering,
    we would have to replicate the vertex coord anyway, because for OpenGL arrays
    we can pass only one index (there is no separate index for coords, normals etc.).
    So we would not save anything (and only increase the work needed for
    OpenGL rendering).

    Besides, texCoordIndex would then have to be equal to normalIndex.
}

  { For given angle, calculate side face normal vector.
    Where Angle is in [0...CalculateQuadricSlices] range,
    we will automatically adjust it for correct angle in radians. }
  function SideNormal(const Angle: Single): TVector3;
  var
    AngleSin, AngleCos: Float;
  begin
    SinCos((Angle / QuadricSlices) * 2 * Pi, AngleSin, AngleCos);

    Result.X := -AngleSin;

    if TopRadius = 0 then
      { Why? Draw a ortho view of cone, and note that two triangles are
        similar, and notice a proportion
        1 / Height = Result.Y / BottomRadius. }
      Result.Y := BottomRadius / Height else
      { This assumes that BottomRadius = TopRadius.
        It's Ok, this procedure is used now only for straight cylinder
        or a cone. }
      Result.Y := 0;
    Result.Z := -AngleCos;

    { When Result.Y is zero (straight cylinder), it's already normalized }
    if Result.Y <> 0 then
      Result := Result.Normalize;
  end;

var
  BottomCircleIndex, BottomMiddleIndex, BottomFaceIndex: Integer;
  TopCircleIndex, TopMiddleIndex, TopFaceIndex: Integer;
  SideBottomCircleIndex, SideTopCircleIndex, SideFaceIndex: Integer;

  { Add additional stack in the middle.
    StackHeight should be between 0 = bottom and 1 = top
    (not inclusive, i.e. don't use this to generate bottom/top,
    this is done by special code.) }
  procedure AddStack(const StackHeight: Single);
  var
    I: Integer;
    MiddleCircleIndex, MiddleFaceIndex: Integer;
  begin
    MiddleCircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices + 1;
    GenerateCircle(QuadricSlices, Lerp(StackHeight, BottomRadius, TopRadius), -Height / 2 + StackHeight * Height, PVector3(Coord.Ptr(MiddleCircleIndex)));
    Coord.L[Coord.Count - 1] := Coord.L[MiddleCircleIndex];
    Normal.Count := Normal.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[MiddleCircleIndex + I] := Normal.L[SideBottomCircleIndex + I];
    Normal.L[Normal.Count - 1] := Normal.L[MiddleCircleIndex];
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
      for I := 0 to QuadricSlices do
        TexCoord.L[MiddleCircleIndex + I] := Vector2(I / QuadricSlices, StackHeight);
    end;

    MiddleFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
    for I := 0 to QuadricSlices - 1 do
    begin
      CoordIndex.L[MiddleFaceIndex + I * 5    ] := MiddleCircleIndex     + I;
      CoordIndex.L[MiddleFaceIndex + I * 5 + 1] := SideBottomCircleIndex + I;
      CoordIndex.L[MiddleFaceIndex + I * 5 + 2] := SideBottomCircleIndex + I + 1;
      CoordIndex.L[MiddleFaceIndex + I * 5 + 3] := MiddleCircleIndex     + I + 1;
      CoordIndex.L[MiddleFaceIndex + I * 5 + 4] := -1;
    end;

    if MaterialIndex <> nil then
      MaterialIndex.AddDuplicate(0, QuadricSlices);

    { Next stack added. Now replace SideBottomCircleIndex,
      to say that the the current circle is the bottom now. }
    SideBottomCircleIndex := MiddleCircleIndex;
  end;

var
  I, J: Integer;
begin
  { For VRML 1.0, some of these MF fields by default have non-empty content.
    It's safest to just clean them. }
  CoordIndex.Count := 0;
  Coord.Count := 0;
  Normal.Count := 0;
  if TexCoord <> nil then TexCoord.Count := 0;
  if MaterialIndex <> nil then MaterialIndex.Count := 0;

  if Bottom then
  begin
    BottomCircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices;
    GenerateCircle(QuadricSlices, BottomRadius, -Height / 2, PVector3(Coord.Ptr(BottomCircleIndex)));
    Normal.Count := Normal.Count + QuadricSlices;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[BottomCircleIndex + I] := Vector3(0, -1, 0);
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices;
      GenerateTexCoordCircle(QuadricSlices, PVector2(TexCoord.Ptr(BottomCircleIndex)), false);
    end;

    BottomMiddleIndex := Coord.Count;
    Coord.Add^ := Vector3(0, -Height / 2, 0);
    Normal.Add^ := Vector3(0, -1, 0);
    if TexCoord <> nil then
      TexCoord.Add^ := Vector2(0.5, 0.5);

    BottomFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
    for I := 0 to QuadricSlices - 1 do
    begin
      if I <> QuadricSlices - 1 then
        CoordIndex.L[BottomFaceIndex + I * 4  ] := BottomCircleIndex + I + 1 else
        CoordIndex.L[BottomFaceIndex + I * 4  ] := BottomCircleIndex;
      CoordIndex.L[BottomFaceIndex + I * 4 + 1] := BottomCircleIndex + I;
      CoordIndex.L[BottomFaceIndex + I * 4 + 2] := BottomMiddleIndex;
      CoordIndex.L[BottomFaceIndex + I * 4 + 3] := -1;
    end;

    if MaterialIndex <> nil then
    begin
      { fill materialIndex for bottom disk.
        For VRML 1.0 Cone (TopRadius = 0), this is material 1,
        for VRML 1.0 Cylinder this is material 2. }
      if TopRadius = 0 then
        MaterialIndex.AddDuplicate(1, QuadricSlices) else
        MaterialIndex.AddDuplicate(2, QuadricSlices);
    end;
  end;

  if Top and (TopRadius <> 0) then
  begin
    TopCircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices;
    GenerateCircle(QuadricSlices, TopRadius, Height / 2, PVector3(Coord.Ptr(TopCircleIndex)));
    Normal.Count := Normal.Count + QuadricSlices;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[TopCircleIndex + I] := Vector3(0, 1, 0);
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices;
      GenerateTexCoordCircle(QuadricSlices, PVector2(TexCoord.Ptr(TopCircleIndex)), true);
    end;

    TopMiddleIndex := Coord.Count;
    Coord.Add^ := Vector3(0, Height / 2, 0);
    Normal.Add^ := Vector3(0, 1, 0);
    if TexCoord <> nil then
      TexCoord.Add^ := Vector2(0.5, 0.5);

    TopFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
    for I := 0 to QuadricSlices - 1 do
    begin
      CoordIndex.L[TopFaceIndex + I * 4    ] := TopCircleIndex + I;
      if I <> QuadricSlices - 1 then
        CoordIndex.L[TopFaceIndex + I * 4 + 1] := TopCircleIndex + I + 1 else
        CoordIndex.L[TopFaceIndex + I * 4 + 1] := TopCircleIndex;
      CoordIndex.L[TopFaceIndex + I * 4 + 2] := TopMiddleIndex;
      CoordIndex.L[TopFaceIndex + I * 4 + 3] := -1;
    end;

    if MaterialIndex <> nil then
    begin
      { fill materialIndex for top disk (this will be called only for VRML 1.0
        Cylinder). }
      MaterialIndex.AddDuplicate(1, QuadricSlices);
    end;
  end;

  if Side then
  begin
    { Generate another circles, instead of reusing Bottom/TopCircleIndex.
      That's because these vertices need different normal vectors anyway,
      so we may as well duplicate their coords.

      We generate QuadricSlices + 1 edges (instead of making QuadricSlices
      edges, and reusing the first edge for the last edge).
      Reason: texture coordinates must be different at the last edge
      (1.0) than the first (0.0).
      Otherwise texture seam would not be correctly closed. }
    SideBottomCircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices + 1;
    GenerateCircle(QuadricSlices, BottomRadius, -Height / 2, PVector3(Coord.Ptr(SideBottomCircleIndex)));
    Coord.L[Coord.Count - 1] := Coord.L[SideBottomCircleIndex];
    Normal.Count := Normal.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[SideBottomCircleIndex + I] := SideNormal(I);
    Normal.L[Normal.Count - 1] := Normal.L[SideBottomCircleIndex];
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
      for I := 0 to QuadricSlices do
        TexCoord.L[SideBottomCircleIndex + I] := Vector2(I / QuadricSlices, 0);
    end;

    { Add additional "stacks" to the geometry when QuadricStacks > 1 }
    for J := 1 to QuadricStacks - 1 do
      AddStack(J / QuadricStacks);
    (* Alt approach, for Cone tip:
    if ExtraStackNearTop then
      AddStack(0.9);
    *)

    SideTopCircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices + 1;
    { Even when TopRadius = 0 (we make a cone), we still generate
      QuadricSlices * top points.
      See comments at the beginning about "How to make smooth normals...". }
    GenerateCircle(QuadricSlices, TopRadius, Height / 2, PVector3(Coord.Ptr(SideTopCircleIndex)));
    Coord.L[Coord.Count - 1] := Coord.L[SideTopCircleIndex];
    Normal.Count := Normal.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices - 1 do
      Normal.L[SideTopCircleIndex + I] := Normal.L[SideBottomCircleIndex + I];
    Normal.L[Normal.Count - 1] := Normal.L[SideTopCircleIndex];
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
      for I := 0 to QuadricSlices do
        TexCoord.L[SideTopCircleIndex + I] := Vector2(I / QuadricSlices, 1);
    end;

    SideFaceIndex := CoordIndex.Count;
    if TopRadius <> 0 then
    begin
      CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
      for I := 0 to QuadricSlices - 1 do
      begin
        CoordIndex.L[SideFaceIndex + I * 5    ] := SideTopCircleIndex    + I;
        CoordIndex.L[SideFaceIndex + I * 5 + 1] := SideBottomCircleIndex + I;
        CoordIndex.L[SideFaceIndex + I * 5 + 2] := SideBottomCircleIndex + I + 1;
        CoordIndex.L[SideFaceIndex + I * 5 + 3] := SideTopCircleIndex    + I + 1;
        CoordIndex.L[SideFaceIndex + I * 5 + 4] := -1;
      end;
    end else
    begin
      { We *could* use the above code, that treats each face as a quad,
        even in case TopRadius = 0. OpenGL rendering works (in practice,
        rects are converted into two triangles and one of them will not
        do anything), octree construction works (degenerate triangle is
        removed). But what's the point in wasting memory (and sending
        to OpenGL dummy triangles) then? }
      CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
      for I := 0 to QuadricSlices - 1 do
      begin
        CoordIndex.L[SideFaceIndex + I * 4    ] := SideBottomCircleIndex + I;
        CoordIndex.L[SideFaceIndex + I * 4 + 1] := SideBottomCircleIndex + I + 1;
        CoordIndex.L[SideFaceIndex + I * 4 + 2] := SideTopCircleIndex    + I + 1;
        CoordIndex.L[SideFaceIndex + I * 4 + 3] := -1;
      end;
    end;

    if MaterialIndex <> nil then
      MaterialIndex.AddDuplicate(0, QuadricSlices);
  end;
end;

{ VRML 2 proxy --------------------------------------------------------------- }

function TCylinderNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  CoordNode: TCoordinateNode;
  NormalNode: TNormalNode;
  TexCoordNode: TTextureCoordinateNode;
  TexCoords: TVector2List;
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

    CylinderCone_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items, TexCoords, nil,
      FdRadius.Value, FdRadius.Value, FdHeight.Value,
      FdBottom.Value, FdSide.Value, FdTop.Value,
      CalculateSlices, CalculateStacks);

    IFS.FdSolid.Value := FdSolid.Value;

    { Smooth everything. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

function TConeNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  CoordNode: TCoordinateNode;
  NormalNode: TNormalNode;
  TexCoordNode: TTextureCoordinateNode;
  TexCoords: TVector2List;
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

    CylinderCone_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items, TexCoords, nil,
      FdBottomRadius.Value, 0, FdHeight.Value,
      FdBottom.Value, FdSide.Value, false,
      CalculateSlices, CalculateStacks);

    IFS.FdSolid.Value := FdSolid.Value;

    { Smooth everything. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

{ VRML 1 proxy --------------------------------------------------------------- }

{$warnings off} // using deprecated constants to keep supporting VRML 1
function TCylinderNode_1.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  CoordNode: TCoordinate3Node_1;
  NormalNode: TNormalNode;
  NormalBinding: TNormalBindingNode_1;
  TexCoordNode: TTextureCoordinate2Node_1;
  ShapeHints: TShapeHintsNode_1;
  MaterialBinding: TMaterialBindingNode_1;
  MaterialIndex: TInt32List;
  IFS: TIndexedFaceSetNode_1 absolute Result;
begin
  IFS := TIndexedFaceSetNode_1.Create(X3DName, BaseUrl);
  try
    { we have to modify State, so make a copy of it }
    State := TX3DGraphTraverseState.CreateCopy(State);

    CoordNode := TCoordinate3Node_1.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsCoordinate3, CoordNode);

    NormalNode := TNormalNode.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsNormal, NormalNode);

    NormalBinding := TNormalBindingNode_1.Create('', BaseUrl);
    { NormalBinding.value = PER_VERTEX means we use niPerVertexCoordIndexed,
      so coordIndex chooses the normal. }
    NormalBinding.FdValue.Value := BIND_PER_VERTEX;
    State.VRML1State.SetOwnNode(vsNormalBinding, NormalBinding);

    TexCoordNode := TTextureCoordinate2Node_1.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsTextureCoordinate2, TexCoordNode);

    ShapeHints := TShapeHintsNode_1.Create('', BaseUrl);
    { For VRML 1.0, Cylinder is never solid. }
    ShapeHints.FdshapeType.Value := SHTYPE_UNKNOWN;
    ShapeHints.FdvertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE;
    { Smooth everything. Not really needed, we use explicit normal node now. }
    ShapeHints.FdCreaseAngle.Value := 4;
    State.VRML1State.SetOwnNode(vsShapeHints, ShapeHints);

    { calculate MaterialBinding node and MaterialIndex }
    MaterialBinding := TMaterialBindingNode_1.Create('', BaseUrl);
    if State.VRML1State.MaterialBinding.FdValue.Value in
      [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    begin
      MaterialIndex := IFS.FdMaterialIndex.Items;
      MaterialBinding.FdValue.Value := BIND_PER_FACE_INDEXED;
    end else
    begin
      MaterialIndex := nil;
      MaterialBinding.FdValue.Value := BIND_OVERALL;
    end;
    State.VRML1State.SetOwnNode(vsMaterialBinding, MaterialBinding);

    CylinderCone_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items,
      TexCoordNode.FdPoint.Items, MaterialIndex,
      FdRadius.Value, FdRadius.Value, FdHeight.Value,
      FdParts.Flags[CYLINDER_PARTS_BOTTOM],
      FdParts.Flags[CYLINDER_PARTS_SIDES],
      FdParts.Flags[CYLINDER_PARTS_TOP],
      DefaultTriangulationSlices,
      { Just like with TCylinderNode.CalculateStacks, Cylinder in VRML 1 doesn't use stacks }
      1);

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex).
      This is a problem of VRML 1.0 "state" idea --- there is no
      other way to "turn off" texture than to just use empty textureCoordIndex. }
    IFS.FdTextureCoordIndex.Items.Assign(IFS.FdCoordIndex.Items);
  except FreeAndNil(Result); raise end;
end;

function TConeNode_1.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  CoordNode: TCoordinate3Node_1;
  NormalNode: TNormalNode;
  NormalBinding: TNormalBindingNode_1;
  TexCoordNode: TTextureCoordinate2Node_1;
  ShapeHints: TShapeHintsNode_1;
  MaterialBinding: TMaterialBindingNode_1;
  MaterialIndex: TInt32List;
  IFS: TIndexedFaceSetNode_1 absolute Result;
begin
  IFS := TIndexedFaceSetNode_1.Create(X3DName, BaseUrl);
  try
    { we have to modify State, so make a copy of it }
    State := TX3DGraphTraverseState.CreateCopy(State);

    CoordNode := TCoordinate3Node_1.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsCoordinate3, CoordNode);

    NormalNode := TNormalNode.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsNormal, NormalNode);

    NormalBinding := TNormalBindingNode_1.Create('', BaseUrl);
    { NormalBinding.value = PER_VERTEX means we use niPerVertexCoordIndexed,
      so coordIndex chooses the normal. }
    NormalBinding.FdValue.Value := BIND_PER_VERTEX;
    State.VRML1State.SetOwnNode(vsNormalBinding, NormalBinding);

    TexCoordNode := TTextureCoordinate2Node_1.Create('', BaseUrl);
    State.VRML1State.SetOwnNode(vsTextureCoordinate2, TexCoordNode);

    ShapeHints := TShapeHintsNode_1.Create('', BaseUrl);
    { For VRML 1.0, Cone is never solid. }
    ShapeHints.FdshapeType.Value := SHTYPE_UNKNOWN;
    ShapeHints.FdvertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE;
    { Smooth everything. Not really needed, we use explicit normal node now. }
    ShapeHints.FdCreaseAngle.Value := 4;
    State.VRML1State.SetOwnNode(vsShapeHints, ShapeHints);

    { calculate MaterialBinding node and MaterialIndex }
    MaterialBinding := TMaterialBindingNode_1.Create('', BaseUrl);
    if State.VRML1State.MaterialBinding.FdValue.Value in
      [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    begin
      MaterialIndex := IFS.FdMaterialIndex.Items;
      MaterialBinding.FdValue.Value := BIND_PER_FACE_INDEXED;
    end else
    begin
      MaterialIndex := nil;
      MaterialBinding.FdValue.Value := BIND_OVERALL;
    end;
    State.VRML1State.SetOwnNode(vsMaterialBinding, MaterialBinding);

    CylinderCone_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items,
      TexCoordNode.FdPoint.Items, MaterialIndex,
      FdBottomRadius.Value, 0, FdHeight.Value,
      FdParts.Flags[CONE_PARTS_BOTTOM], FdParts.Flags[CONE_PARTS_SIDES], false,
      DefaultTriangulationSlices, DefaultTriangulationStacks);

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex).
      This is a problem of VRML 1.0 "state" idea --- there is no
      other way to "turn off" texture than to just use empty textureCoordIndex. }
    IFS.FdTextureCoordIndex.Items.Assign(IFS.FdCoordIndex.Items);
  except FreeAndNil(Result); raise end;
end;
{$warnings on}
