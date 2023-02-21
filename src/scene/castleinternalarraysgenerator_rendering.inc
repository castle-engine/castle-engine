{%MainUnit castleinternalarraysgenerator.pas}
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

{ TArraysGenerator descendants implementing nodes in X3D "Rendering"
  component. }

type
  TPointSet_1Generator = class(TArraysGenerator)
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinate; override;
  end;

  TPointSetGenerator = class(TAbstractColorGenerator)
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinate; override;
  public
    constructor Create(AShape: TShape); override;
  end;

  { Generator for X3D IndexedTriangleSet, TriangleSet, QuadSet nodes.

    QuadSet is treated much like IndexedTriangleSet,
    thanks to TQuadSetNode.InternalTrianglesCoordIndex. }
  TTriangleSetGenerator = class(TAbstractCompleteGenerator)
  strict private
    TriFaceNormal: TVector3;
  protected
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordinate; override;
    procedure PrepareIndexesPrimitives; override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
    class function BumpMappingAllowed: boolean; override;
  end;

  { Generator for X3D IndexedTriangleFanSet and TriangleFanSet node. }
  TTriangleFanSetGenerator = class(TAbstractCompleteGenerator)
  strict private
    TriFaceNormal: TVector3;
    procedure PrepareIndexesCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer);
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinate; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
    class function BumpMappingAllowed: boolean; override;
  end;

  { Generator for X3D IndexedTriangleStripSet and TriangleStripSet nodes.

    Also for Inventor 1.0 IndexedTriangleMesh (since this is almost
    the same thing as IndexedTriangleStripSet, only defined more in
    Inventor/VRML 1.0 conventions). }
  TTriangleStripSetGenerator = class(TAbstractCompleteGenerator)
  strict private
    TriFaceNormal: TVector3;
    VRML1FrontFaceCcw, VRML1CullFace: boolean;
    procedure PrepareIndexesCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer);
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinate; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
    class function BumpMappingAllowed: boolean; override;
  end;

  { Generator for X3D IndexedQuadSet nodes. }
  TQuadSetGenerator = class(TAbstractCompleteGenerator)
  strict private
    QuadFaceNormal: TVector3;
  protected
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordinate; override;
    procedure PrepareIndexesPrimitives; override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
    class function BumpMappingAllowed: boolean; override;
  end;

  { Common class for IndexedLineSet (VRML <= 1.0 and >= 2.0) and
    LineSet (VRML >= 2.0, although specification only since X3D). }
  TAbstractLineSetGenerator = class(TAbstractCompleteGenerator)
  strict private
    procedure PrepareIndexesCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer);
    function LineMode: TLineMode;
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinate; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  { Generator for VRML <= 1.0 TIndexedLineSetNode_1 }
  TIndexedLineSet_1Generator = class(TAbstractLineSetGenerator)
  public
    constructor Create(AShape: TShape); override;
  end;

  { Generator for VRML >= 2.0 TIndexedLineSetNode and X3D TLineSetNode. }
  TLineSetGenerator = class(TAbstractLineSetGenerator)
  public
    constructor Create(AShape: TShape); override;
  protected
    procedure GenerateCoordinateBegin; override;
  end;

{ TPointSet_1Generator -------------------------------------------------------- }

procedure TPointSet_1Generator.GenerateCoordinateBegin;
var
  M: TPhongMaterialInfo;
begin
  inherited;

  { TODO: handle various possible material binding here }

  { TODO: use here Last :
    Normal, NormalBinding, TextureCoordinate2, Texture2.
    For now light is already disabled by Render_Material
    from Render_BindMaterial_1 at the beginning. }

  Arrays.ForceUnlit := true;
  M := State.VRML1State.Material.MaterialInfo(0);
  Arrays.ForcedUnlitColor := Vector4(M.DiffuseColor, M.Opacity);
end;

procedure TPointSet_1Generator.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpPoints;
end;

procedure TPointSet_1Generator.GenerateCoordinate;
var
  I: Integer;
begin
  for I := 0 to CoordCount - 1 do
    GenerateVertex(I);
end;

{ TPointSetGenerator -------------------------------------------------------- }

constructor TPointSetGenerator.Create(AShape: TShape);
var
  Node: TPointSetNode;
begin
  inherited;

  Node := Geometry as TPointSetNode;

  { In VRML 2.0 PointSet is always unlit and not textured.
    Light is already disabled by Render_Material. }

  { PointSet color may come from various places:
    1. Color or ColorRGBA node, for each point, if it's not NULL
    2. Material.emissiveColor, for every point
    3. If no material, we use default WhiteRGB, for every point
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }

  ColorPerVertex := true; // always per-vertex

  Color := nil;
  ColorRGBA := nil;

  if (Node.FdColor.Value <> nil) and
     (Node.FdColor.Value is TColorNode) then
  begin
    ColorNode := TColorNode(Node.FdColor.Value);
    Color := TColorNode(ColorNode).FdColor;
    if (Coord <> nil) and (Color.Count < Coord.Count) then
    begin
      WritelnWarning('VRML/X3D', 'Not enough colors specified for PointSet');
      ColorNode := nil;
      Color := nil;
    end;
  end;

  if (Node.FdColor.Value <> nil) and
     (Node.FdColor.Value is TColorRGBANode) then
  begin
    ColorNode := TColorRGBANode(Node.FdColor.Value);
    ColorRGBA := TColorRGBANode(ColorNode).FdColor;
    if (Coord <> nil) and (ColorRGBA.Count < Coord.Count) then
    begin
      WritelnWarning('VRML/X3D', 'Not enough colors specified for PointSet');
      ColorNode := nil;
      ColorRGBA := nil;
    end;
  end;
end;

procedure TPointSetGenerator.GenerateCoordinateBegin;
var
  M: TMaterialInfo;
begin
  inherited;

  { Follows PointSet spec:
    If the color field is NULL and there is a Material node
    defined for the Appearance node affecting this PointSet node,
    the emissiveColor of the Material node shall be used to draw the points.

    Note that if a Color/ColorRGBA nodes are used, they will override
    the ForcedUnlitColor (because that's how UnlitMaterial), which also satisfies
    PointSet spec about Color/ColorRGBA.

    TODO: This should change for X3D 4 and glTF,
    points can be lit if they have explicit normal vectors.

    https://www.web3d.org/documents/specifications/19775-1/V4.0/Part01/components/rendering.html#PointSet
  }

  Arrays.ForceUnlit := true;
  M := State.MaterialInfo;
  if M <> nil then
    Arrays.ForcedUnlitColor := Vector4(M.EmissiveColor, M.Opacity)
  else
    Arrays.ForcedUnlitColor := White;
end;

procedure TPointSetGenerator.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpPoints;
end;

procedure TPointSetGenerator.GenerateCoordinate;
var
  I: Integer;
begin
  for I := 0 to Coord.Count - 1 do
    GenerateVertex(I);
end;

{ TTriangleSetGenerator ------------------------------------------------------- }

constructor TTriangleSetGenerator.Create(AShape: TShape);
begin
  inherited;

  ColorNode := Geometry.InternalColorNode;
  Color := Geometry.InternalColor;
  ColorRGBA := Geometry.InternalColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TIndexedTriangleSetNode then
  begin
    TexCoordIndex := TIndexedTriangleSetNode(Geometry).FdIndex;
    ColorIndex := TIndexedTriangleSetNode(Geometry).FdIndex;
  end else
  if Geometry is TQuadSetNode then
  begin
    CoordIndex := TQuadSetNode(Geometry).InternalTrianglesCoordIndex;
    TexCoordIndex := TQuadSetNode(Geometry).InternalTrianglesCoordIndex;
    ColorIndex := TQuadSetNode(Geometry).InternalTrianglesCoordIndex;
  end;

  Normals := (Geometry as TAbstractComposedGeometryNode).InternalNormal;
  TangentsFromNode := (Geometry as TAbstractComposedGeometryNode).InternalTangent;
  NormalsCcw := (Geometry as TAbstractComposedGeometryNode).FdCcw.Value;
  if Normals <> nil then
  begin
    { For both TriangleSet and IndexedTriangleSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if CoordIndex <> nil then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry is TIndexedTriangleSetNode) and
     TIndexedTriangleSetNode(Geometry).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth(NormalsCcw);
    NorImplementation := niPerVertexCoordIndexed;
  end else
  begin
    { In this case, per-face normals are generated (for
      TriangleSet and QuadSet normals are always generated per-face, since they
      cannot share vertex indexes anyway).
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;
end;

class function TTriangleSetGenerator.BumpMappingAllowed: boolean;
begin
  Result := true;
end;

procedure TTriangleSetGenerator.GenerateCoordinateBegin;
begin
  inherited;
end;

procedure TTriangleSetGenerator.PrepareIndexesPrimitives;
var
  I: Integer;
begin
  Arrays.Primitive := gpTriangles;

  if CoordIndex <> nil then
  begin
    { indexes are just directly from CoordIndex in this case.
      Only make sure we have only full triangles (although nothing really
      requires or promises that, but for the future). }
    IndexesFromCoordIndex := TGeometryIndexList.Create;
    IndexesFromCoordIndex.Assign(CoordIndex.Items);
    IndexesFromCoordIndex.Count := (IndexesFromCoordIndex.Count div 3) * 3;

    if FacesNeeded then
    begin
      Arrays.Faces := TFaceIndexesList.Create;
      Arrays.Faces.Count := IndexesFromCoordIndex.Count;
      for I := 0 to Arrays.Faces.Count - 1 do
      begin
        Arrays.Faces.List^[I].IndexBegin := (I div 3) * 3;
        Arrays.Faces.List^[I].IndexEnd := ((I div 3) + 1) * 3 -1;
      end;
    end;
  end;
end;

procedure TTriangleSetGenerator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
  begin
    Assert(Arrays.Indexes = nil);
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
  end;
end;

procedure TTriangleSetGenerator.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal
  else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleSetGenerator.GenerateCoordinate;
var
  I: Integer;
begin
  I := 0;

  { X3D spec says "If the Coordinate node does not contain
    a multiple of three coordinate values, the remaining
    vertices shall be ignored.".
    So we silently ignore any vertices above multiple of 3. }
  while I + 2 < CoordCount do
  begin
    if NorImplementation = niNone then
      TriFaceNormal := TriangleNormal(
        GetVertex(I), GetVertex(I + 1), GetVertex(I + 2));

    CalculateTangentVectors(I, I + 1, I + 2);

    GenerateVertex(I    );
    GenerateVertex(I + 1);
    GenerateVertex(I + 2);

    Inc(I, 3);
  end;
end;

{ TTriangleFanSetGenerator ---------------------------------------------------- }

constructor TTriangleFanSetGenerator.Create(AShape: TShape);
begin
  inherited;

  ColorNode := Geometry.InternalColorNode;
  Color := Geometry.InternalColor;
  ColorRGBA := Geometry.InternalColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TIndexedTriangleFanSetNode then
  begin
    TexCoordIndex := TIndexedTriangleFanSetNode(Geometry).FdIndex;
    ColorIndex := TIndexedTriangleFanSetNode(Geometry).FdIndex;
  end;

  Normals := (Geometry as TAbstractComposedGeometryNode).InternalNormal;
  TangentsFromNode := (Geometry as TAbstractComposedGeometryNode).InternalTangent;
  NormalsCcw := (Geometry as TAbstractComposedGeometryNode).FdCcw.Value;
  if Normals <> nil then
  begin
    { For both TriangleFanSet and IndexedTriangleFanSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if Geometry is TIndexedTriangleFanSetNode then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry as TAbstractComposedGeometryNode).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth(NormalsCcw);
    if CoordIndex <> nil then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    { In this case, per-face normals are generated.
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;
end;

class function TTriangleFanSetGenerator.BumpMappingAllowed: boolean;
begin
  Result := true;
end;

procedure TTriangleFanSetGenerator.GenerateCoordinateBegin;
begin
  inherited;
end;

procedure TTriangleFanSetGenerator.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpTriangleFan;
  Arrays.Counts := TCardinalList.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TGeometryIndexList.Create;
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}PrepareIndexesCoordsRange);
end;

procedure TTriangleFanSetGenerator.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  if BeginIndex + 2 < EndIndex then
  begin
    Arrays.Counts.Add(EndIndex - BeginIndex);

    if CoordIndex <> nil then
    begin
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex + 1]);

      while BeginIndex + 2 < EndIndex do
      begin
        IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex + 2]);
        Inc(BeginIndex);
      end;
    end;
  end;
end;

procedure TTriangleFanSetGenerator.GenerateCoordinate;
begin
  Geometry.InternalMakeCoordRanges(State, {$ifdef FPC}@{$endif}GenerateCoordsRange);
end;

procedure TTriangleFanSetGenerator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
end;

procedure TTriangleFanSetGenerator.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleFanSetGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

  procedure FaceNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
    begin
      TriFaceNormal := TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3));
      if not NormalsCcw then
        TriFaceNormal := -TriFaceNormal;
    end;
  end;

var
  FirstIndex: Integer;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    FirstIndex := BeginIndex;

    { Note that normal for the first two vertexes is totally ignored
      by OpenGL. When NorImplementation = niNone we have flat shading,
      and then normal values for 1st two vertexes don't matter.
      We set them
      - for security, to have this memory initialized
        to something predictable.
      - also CalculateTangentVectors will call GetNormal that needs them. }

    FaceNormal(FirstIndex, BeginIndex + 1, BeginIndex + 2);
    CalculateTangentVectors(BeginIndex, BeginIndex + 1, BeginIndex + 2);

    GenerateVertex(BeginIndex);
    GenerateVertex(BeginIndex + 1);

    while BeginIndex + 2 < EndIndex do
    begin
      FaceNormal(FirstIndex, BeginIndex + 1, BeginIndex + 2);
      GenerateVertex(BeginIndex + 2);
      Inc(BeginIndex);
    end;
  end else
    { Note that in case of non-indexed TriangleFanSet, this will even
      cause bad rendering of remaining stuff, as we will not add
      count = 1 or 2 (necessary for omitting these vertexes).
      This is invalid according to X3D spec, so no promise of valid rendering. }
    WritelnWarning('VRML/X3D', 'Triangle fan has less than 3 vertexes.')
end;

{ TTriangleStripSetGenerator -------------------------------------------------- }

constructor TTriangleStripSetGenerator.Create(AShape: TShape);

  procedure CreateForIndexedTriangleMesh_1;
  var
    SH: TShapeHintsNode_1;
    ANode: TIndexedTriangleMeshNode_1;
  begin
    ANode := Geometry as TIndexedTriangleMeshNode_1;

    TexCoordIndex := ANode.FdTextureCoordIndex;

    MaterialIndex := ANode.FdMaterialIndex;
    MaterialBinding := State.VRML1State.MaterialBinding.FdValue.Value;
    UpdateMat1Implementation;

    SH := State.VRML1State.ShapeHints;

    {$warnings off} // using deprecated to keep supporting VRML 1
    { At this place, we assume that VERTORDER_UNKNOWN_ORDERING is actually
      VERTORDER_COUNTERCLOCKWISE.
      (VRML 1 authors should not provide explicit normals at all if ordering
      is unknown.) }
    VRML1FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

    VRML1CullFace :=
      (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
      (SH.FdShapeType.Value = SHTYPE_SOLID);
    {$warnings on}

    NormalIndex := ANode.FdNormalIndex;
    Normals := State.VRML1State.Normal.FdVector.Items;
    NormalsCcw := VRML1FrontFaceCcw;
    NorImplementation := NorImplementationFromVRML1Binding(
      State.VRML1State.NormalBinding.FdValue.Value);
  end;

begin
  inherited;

  if Geometry is TIndexedTriangleMeshNode_1 then
  begin
    CreateForIndexedTriangleMesh_1;
    Exit;
  end;

  { Rest of this constructor initializes for X3D [Indexed]TriangleStripSet }

  ColorNode := Geometry.InternalColorNode;
  Color := Geometry.InternalColor;
  ColorRGBA := Geometry.InternalColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TIndexedTriangleStripSetNode then
  begin
    TexCoordIndex := TIndexedTriangleStripSetNode(Geometry).FdIndex;
    ColorIndex := TIndexedTriangleStripSetNode(Geometry).FdIndex;
  end;

  Normals := (Geometry as TAbstractComposedGeometryNode).InternalNormal;
  TangentsFromNode := (Geometry as TAbstractComposedGeometryNode).InternalTangent;
  NormalsCcw := (Geometry as TAbstractComposedGeometryNode).FdCcw.Value;
  if Normals <> nil then
  begin
    { For both TriangleStripSet and IndexedTriangleStripSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if Geometry is TIndexedTriangleStripSetNode then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry as TAbstractComposedGeometryNode).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth(NormalsCcw);
    if CoordIndex <> nil then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    { In this case, per-face normals are generated.
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;
end;

class function TTriangleStripSetGenerator.BumpMappingAllowed: boolean;
begin
  Result := true;
end;

procedure TTriangleStripSetGenerator.GenerateCoordinateBegin;
begin
  inherited;

  if Geometry is TIndexedTriangleMeshNode_1 then
  begin
    { Already calculated in constructor, pass to Arrays now }
    Arrays.FrontFaceCcw := VRML1FrontFaceCcw;
    Arrays.CullFace := VRML1CullFace;
  end;
end;

procedure TTriangleStripSetGenerator.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpTriangleStrip;
  Arrays.Counts := TCardinalList.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TGeometryIndexList.Create;
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}PrepareIndexesCoordsRange);
end;

procedure TTriangleStripSetGenerator.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  if BeginIndex + 2 < EndIndex then
  begin
    Arrays.Counts.Add(EndIndex - BeginIndex);

    if CoordIndex <> nil then
    begin
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex + 1]);

      while BeginIndex + 2 < EndIndex do
      begin
        IndexesFromCoordIndex.Add(CoordIndex.Items.List^[BeginIndex + 2]);
        Inc(BeginIndex);
      end;
    end;
  end;
end;

procedure TTriangleStripSetGenerator.GenerateCoordinate;
begin
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}GenerateCoordsRange);
end;

procedure TTriangleStripSetGenerator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
end;

procedure TTriangleStripSetGenerator.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleStripSetGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

  procedure FaceNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
    begin
      TriFaceNormal := TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3));
      if not NormalsCcw then
        TriFaceNormal := -TriFaceNormal;
    end;
  end;

var
  NormalOrder: boolean;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    { Note that normal for the first two vertexes is totally ignored
      by OpenGL. When NorImplementation = niNone we have flat shading,
      and then normal values for 1st two vertexes don't matter.
      We set them
      - for security, to have this memory initialized
        to something predictable.
      - also CalculateTangentVectors will call GetNormal that needs them. }

    FaceNormal(BeginIndex, BeginIndex + 1, BeginIndex + 2);
    CalculateTangentVectors(BeginIndex, BeginIndex + 1, BeginIndex + 2);

    GenerateVertex(BeginIndex);
    GenerateVertex(BeginIndex + 1);

    NormalOrder := true;

    while BeginIndex + 2 < EndIndex do
    begin
      if NormalOrder then
        FaceNormal(BeginIndex    , BeginIndex + 1, BeginIndex + 2) else
        FaceNormal(BeginIndex + 1, BeginIndex    , BeginIndex + 2);
      NormalOrder := not NormalOrder;

      GenerateVertex(BeginIndex + 2);

      Inc(BeginIndex);
    end;
  end else
    { Note that in case of non-indexed TriangleStripSet, this will even
      cause bad rendering of remaining stuff, as we will not add
      count = 1 or 2 (necessary for omitting these vertexes).
      This is invalid according to X3D spec, so no promise of valid rendering. }
    WritelnWarning('VRML/X3D', 'Triangle strip has less than 3 vertexes.');
end;

{ TQuadSetGenerator ------------------------------------------------------- }

constructor TQuadSetGenerator.Create(AShape: TShape);
begin
  inherited;

  ColorNode := Geometry.InternalColorNode;
  Color := Geometry.InternalColor;
  ColorRGBA := Geometry.InternalColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  Assert(Geometry is TIndexedQuadSetNode);
  TexCoordIndex := TIndexedQuadSetNode(Geometry).FdIndex;
  ColorIndex := TIndexedQuadSetNode(Geometry).FdIndex;

  { Normals are done exactly like for [Indexed]TriangleSet, except
    we have quads now. }

  Normals := (Geometry as TAbstractComposedGeometryNode).InternalNormal;
  TangentsFromNode := (Geometry as TAbstractComposedGeometryNode).InternalTangent;
  NormalsCcw := (Geometry as TAbstractComposedGeometryNode).FdCcw.Value;
  if Normals <> nil then
  begin
    { For both QuadSet and IndexedQuadSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex),
      as far as I understand X3D spec. }
    {
    if Geometry is TIndexedQuadSetNode then
      NorImplementation := niPerVertexCoordIndexed
    else
      NorImplementation := niPerVertexNonIndexed;
    }

    Assert(Geometry is TIndexedQuadSetNode);
    NorImplementation := niPerVertexCoordIndexed;
  end else
  if {(Geometry is TIndexedQuadSetNode) and}
     TIndexedQuadSetNode(Geometry).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth(NormalsCcw);
    NorImplementation := niPerVertexCoordIndexed;
  end else
  begin
    { In this case, per-face normals are generated.
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;
end;

class function TQuadSetGenerator.BumpMappingAllowed: boolean;
begin
  Result := true;
end;

procedure TQuadSetGenerator.GenerateCoordinateBegin;
begin
  inherited;
end;

procedure TQuadSetGenerator.PrepareIndexesPrimitives;

  { Calculate IndexesFromCoordIndex for triangle primitive
    from CoordIndex for quad primitive. }
  procedure CalculateIndexesFromCoordIndexForTriangles;
  var
    I: Integer;
  begin
    I := 0;
    while I + 3 < CoordCount do
    begin
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I + 1]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I + 2]);

      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I + 2]);
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I + 3]);
      Inc(I, 4);
    end;
  end;

  procedure CalculateCountsForTriangleFan;
  var
    I: Integer;
  begin
    I := 0;
    while I + 3 < CoordCount do
    begin
      Arrays.Counts.Add(4);
      Inc(I, 4);
    end;
  end;

begin
  { On OpenGLES and modern OpenGL ( https://docs.gl/gl3/glDrawArrays ),
    we don't have GL_QUADS primitive. So render quads by splitting them into triangles. }

  Assert(CoordIndex <> nil); // we only handle TIndexedQuadSetNode here

  Arrays.Primitive := gpTriangles;
  IndexesFromCoordIndex := TGeometryIndexList.Create;
  CalculateIndexesFromCoordIndexForTriangles;
end;

procedure TQuadSetGenerator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
  begin
    Assert(Arrays.Indexes = nil);
    Arrays.Normal(ArrayIndexNum)^ := QuadFaceNormal;
  end;
end;

procedure TQuadSetGenerator.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := QuadFaceNormal
  else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TQuadSetGenerator.GenerateCoordinate;
var
  I: Integer;
begin
  I := 0;

  { X3D spec says to silently ignore any vertices above multiple of 4. }
  while I + 3 < CoordCount do
  begin
    if NorImplementation = niNone then
      { Normal is average of normals of two triangles. }
      QuadFaceNormal := (
        TriangleNormal(GetVertex(I), GetVertex(I + 1), GetVertex(I + 2)) +
        TriangleNormal(GetVertex(I), GetVertex(I + 2), GetVertex(I + 3)) ).Normalize;

    CalculateTangentVectors(I, I + 1, I + 2);

    Assert(Arrays.Primitive = gpTriangles);

    { call GenerateVertex in the same order as you places vertexes
      on IndexesFromCoordIndex in CalculateIndexesFromCoordIndexForTriangles. }
    GenerateVertex(I    );
    GenerateVertex(I + 1);
    GenerateVertex(I + 2);

    GenerateVertex(I    );
    GenerateVertex(I + 2);
    GenerateVertex(I + 3);

    Inc(I, 4);
  end;
end;

{ TAbstractLineSetGenerator -------------------------------------------------- }

procedure TAbstractLineSetGenerator.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  Arrays.Counts.Add(EndIndex - BeginIndex);
  if CoordIndex <> nil then
    for I := BeginIndex to EndIndex - 1 do
      IndexesFromCoordIndex.Add(CoordIndex.Items.List^[I]);
end;

function TAbstractLineSetGenerator.LineMode: TLineMode;
begin
  if Geometry is TIndexedLineSetNode then
    Result := TIndexedLineSetNode(Geometry).Mode
  else
  if Geometry is TLineSetNode then
    Result := TLineSetNode(Geometry).Mode
  else
    Result := lmStrip;
end;

procedure TAbstractLineSetGenerator.PrepareIndexesPrimitives;
begin
  case LineMode of
    lmStrip: Arrays.Primitive := gpLineStrip;
    lmLoop : Arrays.Primitive := gpLineLoop;
    lmPair : Arrays.Primitive := gpLines;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('LineMode?');
    {$endif}
  end;

  Arrays.Counts := TCardinalList.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TGeometryIndexList.Create;
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}PrepareIndexesCoordsRange);
end;

procedure TAbstractLineSetGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  inherited;
  for I := BeginIndex to EndIndex - 1 do
    GenerateVertex(I);
end;

procedure TAbstractLineSetGenerator.GenerateCoordinate;
begin
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}GenerateCoordsRange);
end;

{ TIndexedLineSet_1Generator -------------------------------------------------- }

constructor TIndexedLineSet_1Generator.Create(AShape: TShape);
var
  Node: TIndexedLineSetNode_1;
begin
  inherited;

  Node := Geometry as TIndexedLineSetNode_1;

  TexCoordIndex := Node.FdTextureCoordIndex;

  MaterialIndex := Node.FdMaterialIndex;
  MaterialBinding := State.VRML1State.MaterialBinding.FdValue.Value;
  UpdateMat1Implementation;

  NormalIndex := Node.FdNormalIndex;
  Normals := State.VRML1State.Normal.FdVector.Items;
  {$warnings off} // using deprecated to keep supporting VRML 1
  NormalsCcw :=
    State.VRML1State.ShapeHints.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;
  {$warnings on}
  NorImplementation := NorImplementationFromVRML1Binding(
    State.VRML1State.NormalBinding.FdValue.Value);
end;

{ TLineSetGenerator --------------------------------------------------------- }

constructor TLineSetGenerator.Create(AShape: TShape);
var
  NodeLS: TLineSetNode;
  NodeILS: TIndexedLineSetNode;
begin
  inherited;

  if Geometry is TIndexedLineSetNode then
  begin
    NodeILS := Geometry as TIndexedLineSetNode;

    ColorNode := NodeILS.InternalColorNode;
    Color := NodeILS.InternalColor;
    ColorRGBA := NodeILS.InternalColorRGBA;
    ColorPerVertex := NodeILS.FdColorPerVertex.Value;
    ColorIndex := NodeILS.FdColorIndex;
  end else
  begin
    Assert(Geometry is TLineSetNode);
    NodeLS := Geometry as TLineSetNode;

    ColorNode := NodeLS.InternalColorNode;
    Color := NodeLS.InternalColor;
    ColorRGBA := NodeLS.InternalColorRGBA;
    ColorPerVertex := true; { always true for LineSet }
  end;

  { do not leave NorImplementation as niNone,
    only because niNone forces less optimal rendering AllowIndexed=false. }
  NorImplementation := niUnlit;
end;

procedure TLineSetGenerator.GenerateCoordinateBegin;
var
  M: TMaterialInfo;
begin
  inherited;

  { Implement "one color for the whole lineset" case here.

    Follows X3D 3 spec:
    If the color field is NULL and there is a Material defined for the
    Appearance affecting this LineSet, the emissiveColor of the Material
    shall be used to draw the lines.

    https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/rendering.html#LineSet

    TODO: This should change for X3D 4 and glTF,
    lines can be lit if they have explicit normal vectors.

    https://www.web3d.org/documents/specifications/19775-1/V4.0/Part01/components/rendering.html#LineSet

    IndexedLineSet color may come from various places:
    1. Color node, for each vertex or polyline, if it's not NULL
    2. Material.emissiveColor, for whole line
    3. If no material, we use default WhiteRGB, for whole line
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }

  Arrays.ForceUnlit := true;
  M := State.MaterialInfo;
  if M <> nil then
    Arrays.ForcedUnlitColor := Vector4(M.EmissiveColor, M.Opacity)
  else
    Arrays.ForcedUnlitColor := White;
end;
