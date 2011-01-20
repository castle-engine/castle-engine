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

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Rendering"
  component. }

type
  TPointSet_1Renderer = class(TAbstractCoordinateRenderer)
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinate; override;
  end;

  TPointSet_2Renderer = class(TAbstractColorRenderer)
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinate; override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
    function NeedsSetColor: boolean; override;
  end;

  { Renderer for X3D IndexedTriangleSet and TriangleSet nodes. }
  TTriangleSetRenderer = class(TAbstractCompleteRenderer)
  private
    TriFaceNormal: TVector3Single;
  protected
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordinate; override;
    procedure PrepareIndexesPrimitives; override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3Single); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleFanSet and TriangleFanSet node. }
  TTriangleFanSetRenderer = class(TAbstractCompleteRenderer)
  private
    TriFaceNormal: TVector3Single;
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
      out N: TVector3Single); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleStripSet and TriangleStripSet nodes.

    Also for Inventor 1.0 IndexedTriangleMesh (since this is almost
    the same thing as IndexedTriangleStripSet, only defined more in
    Inventor/VRML 1.0 conventions). }
  TTriangleStripSetRenderer = class(TAbstractCompleteRenderer)
  private
    TriFaceNormal: TVector3Single;
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
      out N: TVector3Single); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Renderer for X3D IndexedQuadSet and QuadSet nodes. }
  TQuadSetRenderer = class(TAbstractCompleteRenderer)
  private
    QuadFaceNormal: TVector3Single;
  protected
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordinate; override;
    procedure PrepareIndexesPrimitives; override;
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3Single); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Common class for IndexedLineSet (VRML <= 1.0 and >= 2.0) and
    LineSet (VRML >= 2.0, although specification only since X3D). }
  TLineSetRenderer = class(TAbstractCompleteRenderer)
  private
    procedure PrepareIndexesCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer);
  protected
    procedure PrepareIndexesPrimitives; override;
    procedure GenerateCoordinate; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  { Renderer for VRML <= 1.0 TNodeIndexedLineSet_1 }
  TIndexedLineSet_1Renderer = class(TLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Renderer for VRML >= 2.0 TNodeIndexedLineSet_2 and X3D TNodeLineSet. }
  TLineSet_2Renderer = class(TLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
    function NeedsSetColor: boolean; override;
  protected
    procedure GenerateCoordinateBegin; override;
  end;

{ TPointSet_1Renderer -------------------------------------------------------- }

procedure TPointSet_1Renderer.GenerateCoordinateBegin;
begin
  { TODO: handle various possible material binding here }

  { TODO: use here Last :
    Normal, NormalBinding, TextureCoordinate2, Texture2.
    For now light is already disabled by Render_Material
    from Render_BindMaterial_1 at the beginning. }

  { Render_BindMaterial_1 set unlit color to emissiveColor,
    so correct it now to diffuseColor. }
  Arrays.HasDefaultColor := true;
  Arrays.DefaultColor := State.LastNodes.Material.DiffuseColor4Single(0);
end;

procedure TPointSet_1Renderer.PrepareIndexesPrimitives;
var
  I, BeginIndex, NumPoints: integer;
  Node: TNodePointSet_1;
begin
  Arrays.Primitive := gpPoints;

  Node := Geometry as TNodePointSet_1;
  Node.CalculateRange(State.LastNodes.Coordinate3, BeginIndex, numPoints);

  { Although PointSet is not really indexed, we use IndexesFromCoordIndex
    to implement BeginIndex / NumPoints limits. }
  IndexesFromCoordIndex := TDynLongIntArray.Create;
  for I := BeginIndex to BeginIndex + NumPoints - 1 do
    IndexesFromCoordIndex.Add(I);
end;

procedure TPointSet_1Renderer.GenerateCoordinate;
var
  I, BeginIndex, NumPoints: Integer;
  Node: TNodePointSet_1;
begin
  Node := Geometry as TNodePointSet_1;
  Node.CalculateRange(State.LastNodes.Coordinate3, BeginIndex, numPoints);

  for I := BeginIndex to BeginIndex + NumPoints - 1 do
    GenerateVertex(I);
end;

{ TPointSet_2Renderer -------------------------------------------------------- }

constructor TPointSet_2Renderer.Create(ARenderer: TVRMLGLRenderer);
var
  Node: TNodePointSet_2;
begin
  inherited;

  Node := Geometry as TNodePointSet_2;

  { In VRML 2.0 PointSet is always unlit and not textured.
    Light is already disabled by Render_Material. }

  { PointSet color may come from various places:
    1. Color or ColorRGBA node, for each point, if it's not NULL
    2. Material.emissiveColor, for every point
    3. If no material, we use default White3Single, for every point
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }

  ColorPerVertex := true; // always per-vertex

  Color := nil;
  ColorRGBA := nil;

  if (Node.FdColor.Value <> nil) and
     (Node.FdColor.Value is TNodeColor) then
  begin
    Color := TNodeColor(Node.FdColor.Value).FdColor;
    if (Coord <> nil) and (Color.Count < Coord.Count) then
    begin
      VRMLWarning(vwSerious, 'Not enough colors specified for PointSet');
      Color := nil;
    end;
  end;

  if (Node.FdColor.Value <> nil) and
     (Node.FdColor.Value is TNodeColorRGBA) then
  begin
    ColorRGBA := TNodeColorRGBA(Node.FdColor.Value).FdColor;
    if (Coord <> nil) and (ColorRGBA.Count < Coord.Count) then
    begin
      VRMLWarning(vwSerious, 'Not enough colors specified for PointSet');
      ColorRGBA := nil;
    end;
  end;
end;

procedure TPointSet_2Renderer.GenerateCoordinateBegin;
var
  Material: TNodeMaterial_2;
begin
  { if base class doesn't do SetColor (or color array) already,
    in particular if Color = ColorRGBA = nil,
    then use color for full PointSet following specification. }
  if not (inherited NeedsSetColor) then
  begin
    Arrays.HasDefaultColor := true;
    if State.ShapeNode = nil then
    begin
      VRMLWarning(vwSerious, 'PointSet in VRML >= 2.0 must be specified only inside Shape.geometry (but it''s not)');
      Arrays.DefaultColor := Vector4Single(White3Single, Renderer.Material_BoundOpacity);
    end else
    begin
      Material := State.ShapeNode.Material;
      if Material <> nil then
        Arrays.DefaultColor := Vector4Single(Material.FdEmissiveColor.Value, Renderer.Material_BoundOpacity) else
        Arrays.DefaultColor := Vector4Single(White3Single, Renderer.Material_BoundOpacity);
    end;
  end;

  inherited;
end;

procedure TPointSet_2Renderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpPoints;
end;

function TPointSet_2Renderer.NeedsSetColor: boolean;
begin
  { Even when Color = ColorRGBA = nil, we do SetColor }
  Result := true;
end;

procedure TPointSet_2Renderer.GenerateCoordinate;
var
  I: Integer;
begin
  for I := 0 to Coord.Count - 1 do
    GenerateVertex(I);
end;

{ TTriangleSetRenderer ------------------------------------------------------- }

constructor TTriangleSetRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;

  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  ColorRGBA := (Geometry as TNodeX3DComposedGeometryNode).ColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleSet then
  begin
    TexCoordIndex := TNodeIndexedTriangleSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedTriangleSet(Geometry).FdIndex;
  end;

  Normals := (Geometry as TNodeX3DComposedGeometryNode).NormalItems;
  if Normals <> nil then
  begin
    { FrontFaceCcw is already filled by ancestor }
    NormalsCcw := FrontFaceCcw;

    { For both TriangleSet and IndexedTriangleSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if Geometry is TNodeIndexedTriangleSet then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry is TNodeIndexedTriangleSet) and
     TNodeIndexedTriangleSet(Geometry).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth;
    NormalsCcw := true;
    NorImplementation := niPerVertexCoordIndexed;
  end else
  begin
    { In this case, per-face normals are generated (for non-indexed
      TriangleSet normals are always generated per-face, since they
      cannot share vertex indexes anyway).
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;

  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for triangle sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is still Ok
      (since each face has separate GenerateVertex calls).

    So always smooth shading is actually Ok.
    Only for the sake of optimization we can force flat shading
    in allowed cases. }
  ForceFlatShading := (Color = nil) and (NorImplementation = niNone);
end;

procedure TTriangleSetRenderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpTriangles;

  if CoordIndex <> nil then
  begin
    { indexes are just directly from CoordIndex in this case.
      Only make sure we have only full triangles (although nothing really
      requires or promises that, but for the future). }
    IndexesFromCoordIndex := TDynLongIntArray.Create;
    IndexesFromCoordIndex.Assign(CoordIndex.Items);
    IndexesFromCoordIndex.Count := (IndexesFromCoordIndex.Count div 3) * 3;
  end;
end;

procedure TTriangleSetRenderer.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
  begin
    Assert(Arrays.Indexes = nil);
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
  end;
end;

procedure TTriangleSetRenderer.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3Single);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleSetRenderer.GenerateCoordinate;
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

    I += 3;
  end;
end;

{ TTriangleFanSetRenderer ---------------------------------------------------- }

constructor TTriangleFanSetRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;

  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  ColorRGBA := (Geometry as TNodeX3DComposedGeometryNode).ColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleFanSet then
  begin
    TexCoordIndex := TNodeIndexedTriangleFanSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedTriangleFanSet(Geometry).FdIndex;
  end;

  Normals := (Geometry as TNodeX3DComposedGeometryNode).NormalItems;
  if Normals <> nil then
  begin
    { FrontFaceCcw is already filled by ancestor }
    NormalsCcw := FrontFaceCcw;

    { For both TriangleFanSet and IndexedTriangleFanSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if Geometry is TNodeIndexedTriangleFanSet then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry as TNodeX3DComposedGeometryNode).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth;
    NormalsCcw := true;
    if CoordIndex <> nil then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    { In this case, per-face normals are generated.
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;

  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for triangle sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is NOT correct
      (as vertexes are shared between triangles; so in smooth shading,
      face normals will be incorrectly distributed among vertexes).

    Which means that colors mixed with per-face normals simply don't work.
    Documented on [http://vrmlengine.sourceforge.net/vrml_implementation_status.php]. }
  if (NorImplementation = niNone) and (Color = nil) then
    ForceFlatShading := true else
  begin
    ForceFlatShading := false;
    if NorImplementation = niNone then
      WarningShadingProblems(true, false);
  end;
end;

procedure TTriangleFanSetRenderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpTriangleFan;
  Arrays.Counts := TDynCardinalArray.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TDynLongIntArray.Create;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange);
end;

procedure TTriangleFanSetRenderer.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  if BeginIndex + 2 < EndIndex then
  begin
    Arrays.Counts.Add(EndIndex - BeginIndex);

    if CoordIndex <> nil then
    begin
      IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex]);
      IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex + 1]);

      while BeginIndex + 2 < EndIndex do
      begin
        IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex + 2]);
        Inc(BeginIndex);
      end;
    end;
  end;
end;

procedure TTriangleFanSetRenderer.GenerateCoordinate;
begin
  Geometry.MakeCoordRanges(State, @GenerateCoordsRange);
end;

procedure TTriangleFanSetRenderer.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
end;

procedure TTriangleFanSetRenderer.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3Single);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleFanSetRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

  procedure FaceNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      TriFaceNormal := TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3));
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
    VRMLWarning(vwSerious, 'Triangle fan has less than 3 vertexes.')
end;

{ TTriangleStripSetRenderer -------------------------------------------------- }

constructor TTriangleStripSetRenderer.Create(ARenderer: TVRMLGLRenderer);

  procedure CreateForIndexedTriangleMesh_1;
  var
    SH: TNodeShapeHints;
    ANode: TNodeIndexedTriangleMesh_1;
  begin
    ANode := Geometry as TNodeIndexedTriangleMesh_1;

    TexCoordIndex := ANode.FdTextureCoordIndex;

    MaterialIndex := ANode.FdMaterialIndex;
    MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
    UpdateMat1Implementation;

    SH := State.LastNodes.ShapeHints;

    { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
      (a autorzy VRMLi w ogole nie powinni podawac normali jesli
      nie podadza vertexOrdering innego niz UNKNOWN) }
    FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

    CullBackFaces :=
      (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
      (SH.FdShapeType.Value = SHTYPE_SOLID);

    NormalIndex := ANode.FdNormalIndex;
    Normals := State.LastNodes.Normal.FdVector.Items;
    NormalsCcw := FrontFaceCcw;
    NorImplementation := NorImplementationFromVRML1Binding(
      State.LastNodes.NormalBinding.FdValue.Value);

    { When generating, we always generate flat normals for IndexedTriangleMesh
      (I don't know if I should use anything like "creaseAngle"?) }
    ForceFlatShading := (NorImplementation = niNone) or NormalsFlat;
  end;

begin
  inherited;

  if Geometry is TNodeIndexedTriangleMesh_1 then
  begin
    CreateForIndexedTriangleMesh_1;
    Exit;
  end;

  { Rest of this constructor initializes for X3D [Indexed]TriangleStripSet }

  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  ColorRGBA := (Geometry as TNodeX3DComposedGeometryNode).ColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleStripSet then
  begin
    TexCoordIndex := TNodeIndexedTriangleStripSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedTriangleStripSet(Geometry).FdIndex;
  end;

  Normals := (Geometry as TNodeX3DComposedGeometryNode).NormalItems;
  if Normals <> nil then
  begin
    { FrontFaceCcw is already filled by ancestor }
    NormalsCcw := FrontFaceCcw;

    { For both TriangleStripSet and IndexedTriangleStripSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex), as far as I understand
      X3D spec. }
    if Geometry is TNodeIndexedTriangleStripSet then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry as TNodeX3DComposedGeometryNode).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth;
    NormalsCcw := true;
    if CoordIndex <> nil then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    { In this case, per-face normals are generated.
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;

  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for triangle sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is NOT correct
      (as vertexes are shared between triangles; so in smooth shading,
      face normals will be incorrectly distributed among vertexes).

    Which means that colors mixed with per-face normals simply don't work.
    Documented on [http://vrmlengine.sourceforge.net/vrml_implementation_status.php]. }
  if (NorImplementation = niNone) and (Color = nil) then
    ForceFlatShading := true else
  begin
    ForceFlatShading := false;
    if NorImplementation = niNone then
      WarningShadingProblems(true, false);
  end;
end;

procedure TTriangleStripSetRenderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpTriangleStrip;
  Arrays.Counts := TDynCardinalArray.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TDynLongIntArray.Create;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange);
end;

procedure TTriangleStripSetRenderer.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  if BeginIndex + 2 < EndIndex then
  begin
    Arrays.Counts.Add(EndIndex - BeginIndex);

    if CoordIndex <> nil then
    begin
      IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex]);
      IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex + 1]);

      while BeginIndex + 2 < EndIndex do
      begin
        IndexesFromCoordIndex.Add(CoordIndex.Items[BeginIndex + 2]);
        Inc(BeginIndex);
      end;
    end;
  end;
end;

procedure TTriangleStripSetRenderer.GenerateCoordinate;
begin
  Geometry.MakeCoordRanges(State, @GenerateCoordsRange);
end;

procedure TTriangleStripSetRenderer.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
    Arrays.Normal(ArrayIndexNum)^ := TriFaceNormal;
end;

procedure TTriangleStripSetRenderer.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3Single);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := TriFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TTriangleStripSetRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

  procedure FaceNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      TriFaceNormal := TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3));
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
    VRMLWarning(vwSerious, 'Triangle strip has less than 3 vertexes.');
end;

{ TQuadSetRenderer ------------------------------------------------------- }

constructor TQuadSetRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;

  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  ColorRGBA := (Geometry as TNodeX3DComposedGeometryNode).ColorRGBA;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedQuadSet then
  begin
    TexCoordIndex := TNodeIndexedQuadSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedQuadSet(Geometry).FdIndex;
  end;

  { Normals are done exactly like for [Indexed]TriangleSet, except
    we have quads now. }

  Normals := (Geometry as TNodeX3DComposedGeometryNode).NormalItems;
  if Normals <> nil then
  begin
    { FrontFaceCcw is already filled by ancestor }
    NormalsCcw := FrontFaceCcw;

    { For both QuadSet and IndexedQuadSet, if normals are supplied
      then they are per-vertex (ignoring normalPerVertex),
      as far as I understand X3D spec. }
    if Geometry is TNodeIndexedQuadSet then
      NorImplementation := niPerVertexCoordIndexed else
      NorImplementation := niPerVertexNonIndexed;
  end else
  if (Geometry is TNodeIndexedQuadSet) and
     TNodeIndexedQuadSet(Geometry).FdNormalPerVertex.Value then
  begin
    Normals := Shape.NormalsSmooth;
    NormalsCcw := true;
    NorImplementation := niPerVertexCoordIndexed;
  end else
  begin
    { In this case, per-face normals are generated (for non-indexed
      QuadSet normals are always generated per-face, since they
      cannot share vertex indexes anyway).
      We generate them on the fly in this case, this will be faster. }
    NorImplementation := niNone;
  end;

  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for quad sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is still Ok
      (since each face has separate GenerateVertex calls).

    So always smooth shading is actually Ok.
    Only for the sake of optimization we can force flat shading
    in allowed cases. }
  ForceFlatShading := (Color = nil) and (NorImplementation = niNone);
end;

procedure TQuadSetRenderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpQuads;

  if CoordIndex <> nil then
  begin
    { indexes are just directly from CoordIndex in this case.
      Only make sure we have only full quads (although nothing really
      requires or promises that, but for the future). }
    IndexesFromCoordIndex := TDynLongIntArray.Create;
    IndexesFromCoordIndex.Assign(CoordIndex.Items);
    IndexesFromCoordIndex.Count := (IndexesFromCoordIndex.Count div 4) * 4;
  end;
end;

procedure TQuadSetRenderer.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  if NorImplementation = niNone then
  begin
    Assert(Arrays.Indexes = nil);
    Arrays.Normal(ArrayIndexNum)^ := QuadFaceNormal;
  end;
end;

procedure TQuadSetRenderer.GetNormal(IndexNum: Integer; RangeNumber: Integer;
  out N: TVector3Single);
begin
  if NorImplementation = niNone then
    { TODO: hack, assuming GetNormal is only called for current face }
    N := QuadFaceNormal else
    inherited GetNormal(IndexNum, RangeNumber, N);
end;

procedure TQuadSetRenderer.GenerateCoordinate;
var
  I: Integer;
begin
  I := 0;

  { X3D spec says to silently ignore any vertices above multiple of 4. }
  while I + 3 < CoordCount do
  begin
    if NorImplementation = niNone then
      { Normal is average of normals of two triangles. }
      QuadFaceNormal := Normalized(
        TriangleNormal(GetVertex(I), GetVertex(I + 1), GetVertex(I + 2)) +
        TriangleNormal(GetVertex(I), GetVertex(I + 2), GetVertex(I + 3)) );

    CalculateTangentVectors(I, I + 1, I + 2);

    GenerateVertex(I    );
    GenerateVertex(I + 1);
    GenerateVertex(I + 2);
    GenerateVertex(I + 3);

    I += 4;
  end;
end;

{ TLineSetRenderer ----------------------------------------------------------- }

procedure TLineSetRenderer.PrepareIndexesCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  Arrays.Counts.Add(EndIndex - BeginIndex);
  if CoordIndex <> nil then
    for I := BeginIndex to EndIndex - 1 do
      IndexesFromCoordIndex.Add(CoordIndex.Items[I]);
end;

procedure TLineSetRenderer.PrepareIndexesPrimitives;
begin
  Arrays.Primitive := gpLineStrip;
  Arrays.Counts := TDynCardinalArray.Create;
  if CoordIndex <> nil then
    IndexesFromCoordIndex := TDynLongIntArray.Create;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange);
end;

procedure TLineSetRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  inherited;
  for I := BeginIndex to EndIndex - 1 do
    GenerateVertex(I);
end;

procedure TLineSetRenderer.GenerateCoordinate;
begin
  Geometry.MakeCoordRanges(State, @GenerateCoordsRange);
end;

{ TIndexedLineSet_1Renderer -------------------------------------------------- }

constructor TIndexedLineSet_1Renderer.Create(
  ARenderer: TVRMLGLRenderer);
var
  Node: TNodeIndexedLineSet_1;
begin
  inherited Create(ARenderer);

  Node := Geometry as TNodeIndexedLineSet_1;

  TexCoordIndex := Node.FdTextureCoordIndex;

  MaterialIndex := Node.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
  UpdateMat1Implementation;

  NormalIndex := Node.FdNormalIndex;
  Normals := State.LastNodes.Normal.FdVector.Items;
  NormalsCcw :=
    State.LastNodes.ShapeHints.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;
  NorImplementation := NorImplementationFromVRML1Binding(
    State.LastNodes.NormalBinding.FdValue.Value);
end;

{ TLineSet_2Renderer --------------------------------------------------------- }

constructor TLineSet_2Renderer.Create(
  ARenderer: TVRMLGLRenderer);
var
  NodeLS: TNodeLineSet;
  NodeILS: TNodeIndexedLineSet_2;
begin
  inherited Create(ARenderer);

  if Geometry is TNodeIndexedLineSet_2 then
  begin
    NodeILS := Geometry as TNodeIndexedLineSet_2;

    Color := NodeILS.Color;
    ColorRGBA := NodeILS.ColorRGBA;
    ColorPerVertex := NodeILS.FdColorPerVertex.Value;
    ColorIndex := NodeILS.FdColorIndex;
  end else
  begin
    Assert(Geometry is TNodeLineSet);
    NodeLS := Geometry as TNodeLineSet;

    Color := NodeLS.Color;
    ColorRGBA := NodeLS.ColorRGBA;
    ColorPerVertex := true; { always true for LineSet }
  end;
end;

procedure TLineSet_2Renderer.GenerateCoordinateBegin;
var
  Material: TNodeMaterial_2;
begin
  inherited;

  { Implement "one color for the whole lineset" case here.

    IndexedLineSet color may come from various places:
    1. Color node, for each vertex or polyline, if it's not NULL
    2. Material.emissiveColor, for whole line
    3. If no material, we use default White3Single, for whole line
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }
  if Color = nil then
  begin
    Arrays.HasDefaultColor := true;
    if State.ShapeNode = nil then
    begin
      VRMLWarning(vwSerious, 'LineSet in VRML >= 2.0 must be specified only inside Shape.geometry (but it''s not)');
      Arrays.DefaultColor := Vector4Single(White3Single, Renderer.Material_BoundOpacity)
    end else
    begin
      Material := State.ShapeNode.Material;
      if Material <> nil then
        Arrays.DefaultColor := Vector4Single(Material.FdEmissiveColor.Value, Renderer.Material_BoundOpacity) else
        Arrays.DefaultColor := Vector4Single(White3Single, Renderer.Material_BoundOpacity)
    end;
  end;
end;

function TLineSet_2Renderer.NeedsSetColor: boolean;
begin
  if Color = nil then
    Result := true else
    Result := inherited NeedsSetColor;
end;
