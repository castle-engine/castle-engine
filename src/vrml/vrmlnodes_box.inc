procedure Box_Proxy(CoordIndex: TDynLongIntArray;
  Coord: TDynVector3SingleArray; Normal: TDynVector3SingleArray;
  TexCoord: TDynVector2SingleArray;
  OverTriangulate: boolean; Sizes: TVector3Single;
  KambiTriangulation: TNodeKambiTriangulation);

  { X1, Y1 specify the corner of the 1st vertex, we go CCW on the face
    to the (-X1, -Y1) corner. So the sign of X1, Y1 determine which side
    of the face is CCW.

    @param(TextureSIsY By default, texture S goes along X and texture T
      goes along Y. Pass TextureSIsY = @true to change it.) }
  procedure MakeRect(const ConstCoord: integer; const ConstCoordValue: Single;
    const X1, Y1: Single; const TexX1, TexY1: Single;
    const TextureSIsY: boolean = false);
  var
    FaceIndex, Index, C1, C2: integer;
    X2, Y2, TexX2, TexY2: Single;

    function MakeTexCoord(const S, T: Single): TVector2Single;
    begin
      if TextureSIsY then
      begin
        Result[0] := T;
        Result[1] := S;
      end else
      begin
        Result[0] := S;
        Result[1] := T;
      end;
    end;

  begin
    // TODO: OverTriangulate = true

    Index := Coord.Count;
    Coord.Count := Coord.Count + 4;
    Normal.Count := Normal.Count + 4;
    TexCoord.Count := TexCoord.Count + 4;

    X2 := -X1;
    Y2 := -Y1;
    TexX2 := 1 - TexX1;
    TexY2 := 1 - TexY1;

    RestOf3dCoords(ConstCoord, C1, C2);

    Coord.Items[Index + 0][C1] :=  X1;
    Coord.Items[Index + 0][C2] :=  Y1;
    Coord.Items[Index + 0][ConstCoord] := ConstCoordValue;

    Coord.Items[Index + 1][C1] :=  X2;
    Coord.Items[Index + 1][C2] :=  Y1;
    Coord.Items[Index + 1][ConstCoord] := ConstCoordValue;

    Coord.Items[Index + 2][C1] :=  X2;
    Coord.Items[Index + 2][C2] :=  Y2;
    Coord.Items[Index + 2][ConstCoord] := ConstCoordValue;

    Coord.Items[Index + 3][C1] :=  X1;
    Coord.Items[Index + 3][C2] :=  Y2;
    Coord.Items[Index + 3][ConstCoord] := ConstCoordValue;

    if ConstCoordValue > 0 then
      Normal.Items[Index][ConstCoord] :=  1 else
      Normal.Items[Index][ConstCoord] := -1;
    Normal.Items[Index][C1] := 0;
    Normal.Items[Index][C2] := 0;

    Normal.Items[Index + 1] := Normal.Items[Index];
    Normal.Items[Index + 2] := Normal.Items[Index];
    Normal.Items[Index + 3] := Normal.Items[Index];

    TexCoord.Items[Index + 0] :=  MakeTexCoord(TexX1, TexY1);
    TexCoord.Items[Index + 1] :=  MakeTexCoord(TexX2, TexY1);
    TexCoord.Items[Index + 2] :=  MakeTexCoord(TexX2, TexY2);
    TexCoord.Items[Index + 3] :=  MakeTexCoord(TexX1, TexY2);

    FaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + 5;
    CoordIndex.Items[FaceIndex    ] := Index;
    CoordIndex.Items[FaceIndex + 1] := Index + 1;
    CoordIndex.Items[FaceIndex + 2] := Index + 2;
    CoordIndex.Items[FaceIndex + 3] := Index + 3;
    CoordIndex.Items[FaceIndex + 4] := -1;
  end;

var
  RectDivisions: Cardinal;
begin
  { For VRML 1.0, some of these MF fields by default have non-empty content.
    It's safest to just clean them. }
  CoordIndex.Count := 0;
  Coord.Count := 0;
  Normal.Count := 0;
  TexCoord.Count := 0;

  RectDivisions := KambiTriangulation.RectDivisions;

  Sizes /= 2;

  (*
  TODO: VRML 1.0 materials:
  var
    MatPerPart: boolean;

    const
      { Material numbers, following VRML 1.0 spec for Cube node. }
      MaterialsPerPart: array [0..2, boolean] of integer =
      ( (2, 3), (5, 4), (1, 0) );

    MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
      BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
  *)

  MakeRect(0, -Sizes[0], +Sizes[1], -Sizes[2], 1, 0, true);
  MakeRect(0,  Sizes[0], -Sizes[1], -Sizes[2], 0, 1, true);
  MakeRect(1, -Sizes[1], -Sizes[0], -Sizes[2], 0, 0);
  MakeRect(1,  Sizes[1], +Sizes[0], -Sizes[2], 1, 1);
  MakeRect(2, -Sizes[2], +Sizes[0], -Sizes[1], 0, 0);
  MakeRect(2,  Sizes[2], -Sizes[0], -Sizes[1], 0, 0);
end;

function TNodeBox.Proxy(State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  CoordNode: TNodeCoordinate;
  NormalNode: TNodeNormal;
  TexCoordNode: TNodeTextureCoordinate;
  TexCoords: TDynVector2SingleArray;
  IFS: TNodeIndexedFaceSet_2 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
  try
    CoordNode := TNodeCoordinate.Create('', WWWBasePath);
    IFS.FdCoord.Value := CoordNode;

    NormalNode := TNodeNormal.Create('', WWWBasePath);
    IFS.FdNormal.Value := NormalNode;
    IFS.FdNormalPerVertex.Value := true;

    if (FdTexCoord.Value <> nil) and FdTexCoord.CurrentChildAllowed then
    begin
      { No need for Box_Proxy to create tex coords. }
      IFS.FdTexCoord.Value := FdTexCoord.Value;
      TexCoords := nil;
    end else
    begin
      TexCoordNode := TNodeTextureCoordinate.Create('', WWWBasePath);
      IFS.FdTexCoord.Value := TexCoordNode;
      TexCoords := TexCoordNode.FdPoint.Items;
    end;

    Box_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items, TexCoords,
      OverTriangulate, FdSize.Value,
      State.LastNodes.KambiTriangulation);

    IFS.FdSolid.Value := FdSolid.Value;

    { Smooth nothing. Not really needed, we use explicit normal node now. }
    IFS.FdCreaseAngle.Value := 0;
  except FreeAndNil(Result); raise end;
end;

function TNodeCube_1.Proxy(State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  CoordNode: TNodeCoordinate3;
  NormalNode: TNodeNormal;
  NormalBinding: TNodeNormalBinding;
  TexCoordNode: TNodeTextureCoordinate2;
  ShapeHints: TNodeShapeHints;
  IFS: TNodeIndexedFaceSet_1 absolute Result;
begin
  IFS := TNodeIndexedFaceSet_1.Create(NodeName, WWWBasePath);
  try
    CoordNode := TNodeCoordinate3.Create('', WWWBasePath);
    State.SetLastNodes(vsCoordinate3, CoordNode, true);

    NormalNode := TNodeNormal.Create('', WWWBasePath);
    State.SetLastNodes(vsNormal, NormalNode, true);

    NormalBinding := TNodeNormalBinding.Create('', WWWBasePath);
    { NormalBinding.value = PER_VERTEX means we use niPerVertexCoordIndexed,
      so coordIndex chooses the normal. }
    NormalBinding.FdValue.Value := BIND_PER_VERTEX;
    State.SetLastNodes(vsNormalBinding, NormalBinding, true);

    TexCoordNode := TNodeTextureCoordinate2.Create('', WWWBasePath);
    State.SetLastNodes(vsTextureCoordinate2, TexCoordNode, true);

    ShapeHints := TNodeShapeHints.Create('', '');
    { For VRML 1.0, Box is always solid. }
    ShapeHints.FdshapeType.Value := SHTYPE_SOLID;
    ShapeHints.FdvertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE;
    { Smooth nothing. Not really needed, we use explicit normal node now. }
    ShapeHints.FdCreaseAngle.Value := 0;
    State.SetLastNodes(vsShapeHints, ShapeHints, true);

    Box_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items,
      TexCoordNode.FdPoint.Items,
      OverTriangulate,
      Vector3Single(FdWidth.Value, FdHeight.Value, FdDepth.Value),
      State.LastNodes.KambiTriangulation);

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex).
      This is a problem of VRML 1.0 "state" idea --- there is no
      other way to "turn off" texture than to just use empty textureCoordIndex. }
    IFS.FdTextureCoordIndex.Items.Assign(IFS.FdCoordIndex.Items);
  except FreeAndNil(Result); raise end;
end;
