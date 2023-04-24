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

{ Make a box.

  @param(MaterialIndex VRML 1.0 material index, generated like for
    material binding per part. Will be used with
    material binding BIND_PER_FACE_INDEXED. Pass @nil if you don't
    need it.) }
procedure Box_Proxy(CoordIndex: TInt32List;
  Coord: TVector3List; Normal: TVector3List;
  TexCoord: TVector2List; MaterialIndex: TInt32List;
  Sizes: TVector3);

var
  Detail: TVector3Cardinal;

  { Make a rectangle that changes along the RestOf3DCoords(ConstCoord, ...).
    E.g. for ConstCoord = 0, it changes along YZ (so X below corresponds
    actually to global Y, and Y below corresponds actually to global Z).

    X1, Y1 specify the corner of the 1st vertex, we go CCW on the face
    to the (-X1, -Y1) corner. So the sign of X1, Y1 determine which side
    of the face is CCW.

    @param(TextureSIsY By default, texture S goes along X and texture T
      goes along Y. Pass TextureSIsY = @true to change it.) }
  procedure MakeRect(const ConstCoord: T3DAxis; const ConstCoordValue: Single;
    const X1, Y1: Single; const TexX1, TexY1: Single;
    const FaceMaterialIndex: Integer;
    const TextureSIsY: boolean = false);

    function MakeTexCoord(const S, T: Single): TVector2;
    begin
      if TextureSIsY then
      begin
        Result.X := T;
        Result.Y := S;
      end else
      begin
        Result.X := S;
        Result.Y := T;
      end;
    end;

  var
    FaceIndex, Index: Integer;
    C1, C2: T3DAxis;
    X2, Y2, TexX2, TexY2: Single;
    I, J, NewCoords, ThisIndex: Integer;
    N: TVector3;
  begin
    RestOf3DCoords(ConstCoord, C1, C2);

    Index := Coord.Count;
    NewCoords := (Detail[C1] + 2) * (Detail[C2] + 2);
    Coord.Count := Coord.Count + NewCoords;
    Normal.Count := Normal.Count + NewCoords;
    if TexCoord <> nil then TexCoord.Count := TexCoord.Count + NewCoords;

    X2 := -X1;
    Y2 := -Y1;
    TexX2 := 1 - TexX1;
    TexY2 := 1 - TexY1;

    { Normal vector is constant for the whole rectangle }
    if ConstCoordValue > 0 then
      N.Data[ConstCoord] :=  1
    else
      N.Data[ConstCoord] := -1;
    N.Data[C1] := 0;
    N.Data[C2] := 0;

    for I := 0 to Detail[C1] + 1 do
      for J := 0 to Detail[C2] + 1 do
      begin
        ThisIndex := Index + I * (Detail[C2] + 2) + J;

        Coord.List^[ThisIndex].Data[C1] := Lerp(I / (Detail[C1] + 1), X1, X2);
        Coord.List^[ThisIndex].Data[C2] := Lerp(J / (Detail[C2] + 1), Y1, Y2);
        Coord.List^[ThisIndex].Data[ConstCoord] := ConstCoordValue;

        Normal.List^[ThisIndex] := N;

        if TexCoord <> nil then
          TexCoord.List^[ThisIndex] := MakeTexCoord(
            Lerp(I / (Detail[C1] + 1), TexX1, TexX2),
            Lerp(J / (Detail[C2] + 1), TexY1, TexY2));
      end;

    for I := 1 to Detail[C1] + 1 do
      for J := 1 to Detail[C2] + 1 do
      begin
        FaceIndex := CoordIndex.Count;
        CoordIndex.Count := CoordIndex.Count + 5;
        CoordIndex.List^[FaceIndex    ] := Index + (I - 1) * (Detail[C2] + 2) + J - 1;
        CoordIndex.List^[FaceIndex + 1] := Index +  I      * (Detail[C2] + 2) + J - 1;
        CoordIndex.List^[FaceIndex + 2] := Index +  I      * (Detail[C2] + 2) + J;
        CoordIndex.List^[FaceIndex + 3] := Index + (I - 1) * (Detail[C2] + 2) + J;
        CoordIndex.List^[FaceIndex + 4] := -1;
      end;

    if MaterialIndex <> nil then
      MaterialIndex.AddDuplicate(FaceMaterialIndex, (Detail[C1] + 1) * (Detail[C2] + 1));
  end;

begin
  { For VRML 1.0, some of these MF fields by default have non-empty content.
    It's safest to just clean them. }
  CoordIndex.Count := 0;
  Coord.Count := 0;
  Normal.Count := 0;
  if TexCoord <> nil then TexCoord.Count := 0;
  if MaterialIndex <> nil then MaterialIndex.Count := 0;

  { Currently, all three Detail* used are always equal.
    Although the code is written to support different Detail* values,
    just in case in the future it will be useful.

    TODO: simplify the surrounding code, because right now
    Detail is always 0. }
  Detail := TVector3Cardinal.Zero;

  Sizes := Sizes / 2;

  MakeRect(0, -Sizes.X, +Sizes.Y, -Sizes.Z, 1, 0, 2, true);
  MakeRect(0,  Sizes.X, -Sizes.Y, -Sizes.Z, 0, 1, 3, true);
  MakeRect(1, -Sizes.Y, -Sizes.X, -Sizes.Z, 0, 0, 5);
  MakeRect(1,  Sizes.Y, +Sizes.X, -Sizes.Z, 1, 1, 4);
  MakeRect(2, -Sizes.Z, +Sizes.X, -Sizes.Y, 0, 0, 1);
  MakeRect(2,  Sizes.Z, -Sizes.X, -Sizes.Y, 0, 0, 0);
end;

{ Make a wireframe box. }
procedure BoxWireframe_Proxy(CoordIndex: TInt32List;
  Coord: TVector3List; Sizes: TVector3);
begin
  Sizes := Sizes / 2;

  Coord.AddRange([
    Vector3(-Sizes.X, -Sizes.Y, -Sizes.Z),
    Vector3( Sizes.X, -Sizes.Y, -Sizes.Z),
    Vector3( Sizes.X,  Sizes.Y, -Sizes.Z),
    Vector3(-Sizes.X,  Sizes.Y, -Sizes.Z),

    Vector3(-Sizes.X, -Sizes.Y,  Sizes.Z),
    Vector3( Sizes.X, -Sizes.Y,  Sizes.Z),
    Vector3( Sizes.X,  Sizes.Y,  Sizes.Z),
    Vector3(-Sizes.X,  Sizes.Y,  Sizes.Z)
  ]);

  // Each pair of indexes forms a line, matching TIndexedLineSetNode with Mode = lmPair
  CoordIndex.AddRange([
    // line loop on Z = min
    0, 1,
    1, 2,
    2, 3,
    3, 0,

    // line loop on Z = max
    4, 5,
    5, 6,
    6, 7,
    7, 4,

    // connect Z = min with Z = max
    0, 4,
    1, 5,
    2, 6,
    3, 7
  ]);
end;

function TBoxNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;

  procedure BoxWireframe;
  var
    CoordNode: TCoordinateNode;
    ILS: TIndexedLineSetNode absolute Result;
  begin
    ILS := TIndexedLineSetNode.Create(X3DName, BaseUrl);
    try
      CoordNode := TCoordinateNode.Create('', BaseUrl);
      ILS.FdCoord.Value := CoordNode;
      ILS.Mode := lmPair;

      BoxWireframe_Proxy(ILS.FdCoordIndex.Items, CoordNode.FdPoint.Items,
        FdSize.Value);
    except FreeAndNil(Result); raise end;
  end;

  procedure BoxFilled;
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
        { No need for Box_Proxy to create tex coords. }
        IFS.FdTexCoord.Value := FdTexCoord.Value;
        TexCoords := nil;
      end else
      begin
        TexCoordNode := TTextureCoordinateNode.Create('', BaseUrl);
        IFS.FdTexCoord.Value := TexCoordNode;
        TexCoords := TexCoordNode.FdPoint.Items;
      end;

      Box_Proxy(IFS.FdCoordIndex.Items,
        CoordNode.FdPoint.Items, NormalNode.FdVector.Items, TexCoords, nil,
        FdSize.Value);

      IFS.FdSolid.Value := FdSolid.Value;
      IFS.FdCcw.Value := FdCcw.Value;

      { Box_Proxy generated normals from CCW,
        we need to flip them if CW is front face }
      if not FdCcw.Value then
        NormalNode.FdVector.Items.Negate;

      { Smooth nothing. Not really needed, we use explicit normal node now. }
      IFS.FdCreaseAngle.Value := 0;
    except FreeAndNil(Result); raise end;
  end;

begin
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Shading = shWireframe) then
    BoxWireframe
  else
    BoxFilled;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TCubeNode_1.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
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
    { For VRML 1.0, Box is never solid. }
    ShapeHints.FdshapeType.Value := SHTYPE_UNKNOWN;
    ShapeHints.FdvertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE;
    { Smooth nothing. Not really needed, we use explicit normal node now. }
    ShapeHints.FdCreaseAngle.Value := 0;
    State.VRML1State.SetOwnNode(vsShapeHints, ShapeHints);

    { calculate MaterialBinding node and MaterialIndex }
    MaterialBinding := TMaterialBindingNode_1.Create('', BaseUrl);
    if State.VRML1State.MaterialBinding.FdValue.Value in
      [BIND_PER_PART, BIND_PER_PART_INDEXED,
       BIND_PER_FACE, BIND_PER_FACE_INDEXED] then
    begin
      MaterialIndex := IFS.FdMaterialIndex.Items;
      MaterialBinding.FdValue.Value := BIND_PER_FACE_INDEXED;
    end else
    begin
      MaterialIndex := nil;
      MaterialBinding.FdValue.Value := BIND_OVERALL;
    end;
    State.VRML1State.SetOwnNode(vsMaterialBinding, MaterialBinding);

    Box_Proxy(IFS.FdCoordIndex.Items,
      CoordNode.FdPoint.Items, NormalNode.FdVector.Items,
      TexCoordNode.FdPoint.Items, MaterialIndex,
      Vector3(FdWidth.Value, FdHeight.Value, FdDepth.Value));

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex).
      This is a problem of VRML 1.0 "state" idea --- there is no
      other way to "turn off" texture than to just use empty textureCoordIndex. }
    IFS.FdTextureCoordIndex.Items.Assign(IFS.FdCoordIndex.Items);
  except FreeAndNil(Result); raise end;
end;
{$warnings on}
