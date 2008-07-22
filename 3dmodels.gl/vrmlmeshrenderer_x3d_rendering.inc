{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Rendering"
  component. }

type
  TPointSet_1Renderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  end;

  TPointSet_2Renderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  end;

  { Renderer for X3D IndexedTriangleSet and TriangleSet nodes. }
  TTriangleSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleFanSet and TriangleFanSet node. }
  TTriangleFanSetRenderer = class(TAbstractColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleStripSet and TriangleStripSet node.

    TODO: Inventor's IndexedTriangleMesh_1 should also be
    renderer by this, obsoleting TIndexedTriangleMesh_1Renderer. }
  TTriangleStripSetRenderer = class(TAbstractColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for X3D IndexedQuadSet and QuadSet nodes. }
  TQuadSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TIndexedTriangleMesh_1Renderer = class(TAbstractNorColMatTexCoordinateRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedTriangleMesh_1);
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  { Common class for IndexedLineSet (VRML <= 1.0 and >= 2.0) and
    LineSet (VRML >= 2.0, although specification only since X3D). }
  TLineSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  { Renderer for VRML <= 1.0 TNodeIndexedLineSet_1 }
  TIndexedLineSet_1Renderer = class(TLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for VRML >= 2.0 TNodeIndexedLineSet_2 and X3D TNodeLineSet. }
  TLineSet_2Renderer = class(TLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  protected
    procedure RenderCoordinateBegin; override;
  end;

{ TPointSet_1Renderer -------------------------------------------------------- }

procedure TPointSet_1Renderer.DoRender;
var
  i, BeginIndex, numPoints: integer;
  Node: TNodePointSet_1;
begin
  Node := Geometry as TNodePointSet_1;
  { TODO: handle various possible material binding here }
  Renderer.Render_BindMaterial_1(0);

  { TODO: use here Last :
    Normal, NormalBinding, TextureCoordinate2, Texture2.
    For now light is already disabled by Render_Material
    from Render_BindMaterial_1 above. }

  { Render_BindMaterial_1 set unlit color to emissiveColor,
    so we correct it now. }
  if (not Attributes.PureGeometry) and
     Attributes.ControlMaterials then
    with State.LastNodes.Material do
      glColorv(Vector4f(Attributes.ColorModulated(
        DiffuseColor3Single(0)), Opacity(0)));

  Node.CalculateRange(State.LastNodes.Coordinate3, BeginIndex, numPoints);
  glBegin(GL_POINTS);
  for i := BeginIndex to BeginIndex+numPoints-1 do
    DoGLVertex(State.LastNodes.Coordinate3.FdPoint.Items.Items[i]);
  glEnd;
end;

{ TPointSet_2Renderer -------------------------------------------------------- }

procedure TPointSet_2Renderer.DoRender;
var
  Node: TNodePointSet_2;
var
  Coord: TMFVec3f;
  Color: TMFColor;
  Material: TNodeMaterial_2;
  I: Integer;
begin
  Node := Geometry as TNodePointSet_2;

  { In VRML 2.0 PointSet is always unlit and not textured.
    Light is already disabled by Render_Material. }

  if (Node.FdCoord.Value <> nil) and
     (Node.FdCoord.Value is TNodeCoordinate) then
  begin
    Coord := TNodeCoordinate(Node.FdCoord.Value).FdPoint;

    { PointSet color may come from various places:
      1. Color node, for each point, if it's not NULL
      2. Material.emissiveColor, for every point
      3. If no material, we use default White3Single, for every point
         (following general spec remark at Material node that
         Material = NULL makes unlit white color) }
    Color := nil;
    if (Node.FdColor.Value <> nil) and
       (Node.FdColor.Value is TNodeColor) then
    begin
      Color := TNodeColor(Node.FdColor.Value).FdColor;
      if Color.Count < Coord.Count then
      begin
        VRMLNonFatalError('Not enough colors specified for PointSet');
        Color := nil;
      end;
    end;

    if Color = nil then
    begin
      Material := State.ParentShape.Material;
      if Material <> nil then
        Renderer.SetColor_2(Material.FdEmissiveColor.Value) else
        Renderer.SetColor_2(White3Single);
    end;

    glBegin(GL_POINTS);
    try
      for I := 0 to Coord.Count - 1 do
      begin
        if Color <> nil then
          Renderer.SetColor_2(Color.Items.Items[I]);
        DoGLVertex(Coord.Items.Items[i]);
      end;
    finally glEnd end;
  end;
end;

{ TTriangleSetRenderer ------------------------------------------------------- }

constructor TTriangleSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  Coord := (Geometry as TNodeX3DComposedGeometryNode).Coord;
  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleSet then
  begin
    CoordIndex := TNodeIndexedTriangleSet(Geometry).FdIndex;
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
    Normals := CreateSmoothNormalsTriangleSet(CoordIndex.Items,
      Coord.Items, true);
    NormalsFree := true;
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
end;

procedure TTriangleSetRenderer.RenderCoordinate;
var
  I: Integer;
  ForceFlatShading: boolean;
begin
  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for triangle sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is still Ok
      (since each face has separate RenderVertex calls).

    So always smooth shading is actually Ok.
    Only for the sake of optimization we can force flat shading
    in allowed cases. }
  ForceFlatShading := (Color = nil) and (NorImplementation = niNone);
  if ForceFlatShading then
  begin
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);
  end;

  I := 0;
  glBegin(GL_TRIANGLES);
    { X3D spec says "If the Coordinate node does not contain
      a multiple of three coordinate values, the remaining
      vertices shall be ignored.".
      So we silently ignore any vertices above multiple of 3. }
    while I + 2 < CoordCount do
    begin
      if NorImplementation = niNone then
      begin
        glNormalv(TriangleNormal(
          GetVertex(I    ),
          GetVertex(I + 1),
          GetVertex(I + 2)));
      end;

      RenderVertex(I    );
      RenderVertex(I + 1);
      RenderVertex(I + 2);

      I += 3;
    end;
  glEnd();

  if ForceFlatShading then
  begin
    glPopAttrib;
  end;
end;

{ TTriangleFanSetRenderer ---------------------------------------------------- }

constructor TTriangleFanSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  Coord := (Geometry as TNodeX3DComposedGeometryNode).Coord;
  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleFanSet then
  begin
    CoordIndex := TNodeIndexedTriangleFanSet(Geometry).FdIndex;
    TexCoordIndex := TNodeIndexedTriangleFanSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedTriangleFanSet(Geometry).FdIndex;
  end;
end;

procedure TTriangleFanSetRenderer.RenderCoordinate;
var
  FanCount: TDynLongIntArray;
  RangeNumber, BeginIndex, EndIndex: Integer;
begin
  if CoordIndex <> nil then
    RenderCoordsRanges else
  begin
    FanCount := (Geometry as TNodeTriangleFanSet).FdFanCount.Items;
    EndIndex := 0;
    for RangeNumber := 0 to FanCount.Count - 1 do
    begin
      BeginIndex := EndIndex;
      EndIndex := BeginIndex + FanCount.Items[RangeNumber];
      { Note that EndIndex *may* be equal to CoordCount,
        as EndIndex is not taken into account by RenderCoordsRange. }
      if EndIndex > CoordCount then
      begin
        VRMLNonFatalError('Too much fans (not enough coordinates) in ' + Geometry.NodeTypeName);
        Break;
      end;
      if EndIndex - BeginIndex >= 3 then
        RenderCoordsRange(RangeNumber, BeginIndex, EndIndex) else
        VRMLNonFatalError('Fan count is less than 3 in ' + Geometry.NodeTypeName);
    end;
  end;
end;

procedure TTriangleFanSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
    glNormalv(TriangleNormal(
      GetVertex(V1),
      GetVertex(V2),
      GetVertex(V3)));
  end;

begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    { TODO: for now we always generate flat normals }
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);

      glBegin(GL_TRIANGLE_FAN);

      RenderVertex(BeginIndex);
      RenderVertex(BeginIndex + 1);

      while BeginIndex + 2 < EndIndex do
      begin
        DoNormal(BeginIndex, BeginIndex + 1, BeginIndex + 2);
        RenderVertex(BeginIndex + 2);
        Inc(BeginIndex);
      end;

      glEnd;

    glPopAttrib();
  end;
end;

{ TTriangleStripSetRenderer -------------------------------------------------- }

constructor TTriangleStripSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  Coord := (Geometry as TNodeX3DComposedGeometryNode).Coord;
  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedTriangleStripSet then
  begin
    CoordIndex := TNodeIndexedTriangleStripSet(Geometry).FdIndex;
    TexCoordIndex := TNodeIndexedTriangleStripSet(Geometry).FdIndex;
    ColorIndex := TNodeIndexedTriangleStripSet(Geometry).FdIndex;
  end;
end;

procedure TTriangleStripSetRenderer.RenderCoordinate;
var
  StripCount: TDynLongIntArray;
  RangeNumber, BeginIndex, EndIndex: Integer;
begin
  if CoordIndex <> nil then
    RenderCoordsRanges else
  begin
    StripCount := (Geometry as TNodeTriangleStripSet).FdStripCount.Items;
    EndIndex := 0;
    for RangeNumber := 0 to StripCount.Count - 1 do
    begin
      BeginIndex := EndIndex;
      EndIndex := BeginIndex + StripCount.Items[RangeNumber];
      { Note that EndIndex *may* be equal to CoordCount,
        as EndIndex is not taken into account by RenderCoordsRange. }
      if EndIndex > CoordCount then
      begin
        VRMLNonFatalError('Too much strips (not enough coordinates) in ' + Geometry.NodeTypeName);
        Break;
      end;
      if EndIndex - BeginIndex >= 3 then
        RenderCoordsRange(RangeNumber, BeginIndex, EndIndex) else
        VRMLNonFatalError('Strip count is less than 3 in ' + Geometry.NodeTypeName);
    end;
  end;
end;

procedure TTriangleStripSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
    glNormalv(TriangleNormal(
      GetVertex(V1),
      GetVertex(V2),
      GetVertex(V3)));
  end;

var
  NormalOrder: boolean;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    { TODO: for now we always generate flat normals }
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);

      glBegin(GL_TRIANGLE_STRIP);

      RenderVertex(BeginIndex);
      RenderVertex(BeginIndex + 1);

      NormalOrder := true;

      while BeginIndex + 2 < EndIndex do
      begin
        if NormalOrder then
          DoNormal(BeginIndex    , BeginIndex + 1, BeginIndex + 2) else
          DoNormal(BeginIndex + 1, BeginIndex    , BeginIndex + 2);
        RenderVertex(BeginIndex + 2);
        Inc(BeginIndex);
        NormalOrder := not NormalOrder;
      end;

      glEnd;

    glPopAttrib();
  end;
end;

{ TQuadSetRenderer ------------------------------------------------------- }

constructor TQuadSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  Coord := (Geometry as TNodeX3DComposedGeometryNode).Coord;
  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
  { According to X3D spec, "the value of the colorPerVertex field
    is ignored and always treated as TRUE" }
  ColorPerVertex := true;

  if Geometry is TNodeIndexedQuadSet then
  begin
    CoordIndex := TNodeIndexedQuadSet(Geometry).FdIndex;
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
    Normals := CreateSmoothNormalsQuadSet(CoordIndex.Items,
      Coord.Items, true);
    NormalsFree := true;
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
end;

procedure TQuadSetRenderer.RenderCoordinate;
var
  I: Integer;
  ForceFlatShading: boolean;
begin
  { About shading:
    - we have to use smooth shading if we have colors
      (colors are always per vertex for quad sets).
    - we have to use smooth shading if we have normals per vertex
    - if we have normals per face, smooth shading is still Ok
      (since each face has separate RenderVertex calls).

    So always smooth shading is actually Ok.
    Only for the sake of optimization we can force flat shading
    in allowed cases. }
  ForceFlatShading := (Color = nil) and (NorImplementation = niNone);
  if ForceFlatShading then
  begin
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);
  end;

  I := 0;
  glBegin(GL_QUADS);
    { X3D spec says to silently ignore any vertices above multiple of 4. }
    while I + 3 < CoordCount do
    begin
      if NorImplementation = niNone then
      begin
        { Normal is average of normals of two triangles. }
        glNormalv(VectorAdd(
          TriangleNormal(GetVertex(I    ), GetVertex(I + 1), GetVertex(I + 2)),
          TriangleNormal(GetVertex(I    ), GetVertex(I + 2), GetVertex(I + 3))));
      end;

      RenderVertex(I    );
      RenderVertex(I + 1);
      RenderVertex(I + 2);
      RenderVertex(I + 3);

      I += 4;
    end;
  glEnd();

  if ForceFlatShading then
  begin
    glPopAttrib;
  end;
end;

{ TIndexedTriangleMesh_1Renderer --------------------------------------------- }

constructor TIndexedTriangleMesh_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedTriangleMesh_1);
var
  SH: TNodeShapeHints;
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := State.LastNodes.Coordinate3.FdPoint;

  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2.FdPoint;

  MaterialIndex := ANode.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;

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
end;

procedure TIndexedTriangleMesh_1Renderer.RenderCoordinate;
begin
  RenderCoordsRanges;
end;

procedure TIndexedTriangleMesh_1Renderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

{ TODO: this is just a copy of TTriangleStripSetRenderer.RenderCoordsRange,
  and should be deleted (once all our ancestors functionality will be
  moved to TTriangleStripSetRenderer and TAbstractCoordinateRenderer. }

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
    begin
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      glNormalv(TriangleNormal(
        GetVertex(V1),
        GetVertex(V2),
        GetVertex(V3)));
    end;
  end;

var
  NormalOrder: boolean;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    { TODO: for now we always generate flat normals }
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);

      glBegin(GL_TRIANGLE_STRIP);

      RenderVertex(BeginIndex);
      RenderVertex(BeginIndex + 1);

      NormalOrder := true;

      while BeginIndex + 2 < EndIndex do
      begin
        if NormalOrder then
          DoNormal(BeginIndex    , BeginIndex + 1, BeginIndex + 2) else
          DoNormal(BeginIndex + 1, BeginIndex    , BeginIndex + 2);
        RenderVertex(BeginIndex + 2);
        Inc(BeginIndex);
        NormalOrder := not NormalOrder;
      end;

      glEnd;

    glPopAttrib();
  end;
end;

{ TIndexedLineSetRenderer -------------------------------------------------- }

procedure TLineSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
var i: integer;
begin
  inherited;

  glBegin(GL_LINE_STRIP);
  for i := BeginIndex to endIndex - 1 do RenderVertex(i);
  glEnd;
end;

procedure TLineSetRenderer.RenderCoordinate;
var
  VertexCount: TDynLongIntArray;
  RangeNumber, BeginIndex, EndIndex: Integer;
begin
  if CoordIndex <> nil then
    RenderCoordsRanges else
  begin
    Assert(Geometry is TNodeLineSet);
    VertexCount := (Geometry as TNodeLineSet).FdVertexCount.Items;
    EndIndex := 0;
    for RangeNumber := 0 to VertexCount.Count - 1 do
    begin
      BeginIndex := EndIndex;
      EndIndex := BeginIndex + VertexCount.Items[RangeNumber];
      { Note that EndIndex *may* be equal to CoordCount,
        as EndIndex is not taken into account by RenderCoordsRange. }
      if EndIndex > CoordCount then
      begin
        VRMLNonFatalError('Too much lines (not enough coordinates) in ' + Geometry.NodeTypeName);
        Break;
      end;
      if EndIndex - BeginIndex >= 2 then
        RenderCoordsRange(RangeNumber, BeginIndex, EndIndex) else
        VRMLNonFatalError('Vertex count is less than 2 in ' + Geometry.NodeTypeName);
    end;
  end;
end;

{ TIndexedLineSet_1Renderer -------------------------------------------------- }

constructor TIndexedLineSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeIndexedLineSet_1;
begin
  inherited Create(ARenderer);

  Node := Geometry as TNodeIndexedLineSet_1;

  CoordIndex := Node.FdCoordIndex;
  Coord := State.LastNodes.Coordinate3.FdPoint;

  TexCoordIndex := Node.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2.FdPoint;

  MaterialIndex := Node.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;

  NormalIndex := Node.FdNormalIndex;
  Normals := State.LastNodes.Normal.FdVector.Items;
  NormalsCcw :=
    State.LastNodes.ShapeHints.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;
  NorImplementation := NorImplementationFromVRML1Binding(
    State.LastNodes.NormalBinding.FdValue.Value);
end;

{ TLineSet_2Renderer --------------------------------------------------------- }

constructor TLineSet_2Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer);
var
  NodeLS: TNodeLineSet;
  NodeILS: TNodeIndexedLineSet_2;
begin
  inherited Create(ARenderer);

  if Geometry is TNodeIndexedLineSet_2 then
  begin
    NodeILS := Geometry as TNodeIndexedLineSet_2;

    CoordIndex := NodeILS.FdCoordIndex;
    Coord := NodeILS.Coord;

    Color := NodeILS.Color;
    ColorPerVertex := NodeILS.FdColorPerVertex.Value;
    ColorIndex := NodeILS.FdColorIndex;
  end else
  begin
    Assert(Geometry is TNodeLineSet);
    NodeLS := Geometry as TNodeLineSet;

    Coord := NodeLS.Coord;

    Color := NodeLS.Color;
    ColorPerVertex := true; { always true for LineSet }
  end;
end;

procedure TLineSet_2Renderer.RenderCoordinateBegin;
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
    Material := State.ParentShape.Material;
    if Material <> nil then
      Renderer.SetColor_2(Material.FdEmissiveColor.Value) else
      Renderer.SetColor_2(White3Single);
  end;
end;

