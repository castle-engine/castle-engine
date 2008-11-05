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
  public
    function NeedsSetColor: boolean; override;
  end;

  { Renderer for X3D IndexedTriangleSet and TriangleSet nodes. }
  TTriangleSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleFanSet and TriangleFanSet node. }
  TTriangleFanSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Renderer for X3D IndexedTriangleStripSet and TriangleStripSet nodes.

    Also for Inventor 1.0 IndexedTriangleMesh (since this is almost
    the same thing as IndexedTriangleStripSet, only defined more in
    Inventor/VRML 1.0 conventions). }
  TTriangleStripSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
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
    function NeedsSetColor: boolean; override;
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
      if State.ParentShape = nil then
      begin
        VRMLNonFatalError('PointSet in VRML >= 2.0 must be specified only inside Shape.geometry (but it''s not)');
        Renderer.SetColor(White3Single);
      end else
      begin
        Material := State.ParentShape.Material;
        if Material <> nil then
          Renderer.SetColor(Material.FdEmissiveColor.Value) else
          Renderer.SetColor(White3Single);
      end;
    end;

    glBegin(GL_POINTS);
    try
      for I := 0 to Coord.Count - 1 do
      begin
        if Color <> nil then
          Renderer.SetColor(Color.Items.Items[I]);
        DoGLVertex(Coord.Items.Items[i]);
      end;
    finally glEnd end;
  end;
end;

function TPointSet_2Renderer.NeedsSetColor: boolean;
begin
  Result := true;
end;

{ TTriangleSetRenderer ------------------------------------------------------- }

constructor TTriangleSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
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
    Normals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
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
end;

procedure TTriangleSetRenderer.RenderCoordinate;
var
  I: Integer;
begin
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
end;

{ TTriangleFanSetRenderer ---------------------------------------------------- }

constructor TTriangleFanSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
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
    Normals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
    NormalsFree := true;
    NormalsCcw := true;
    NorImplementation := niPerVertexCoordIndexed;
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

procedure TTriangleFanSetRenderer.RenderCoordinate;
begin
  Geometry.MakeCoordRanges(State, @RenderCoordsRange);
end;

procedure TTriangleFanSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      glNormalv(TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3)));
  end;

var
  FirstIndex: Integer;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
    glBegin(GL_TRIANGLE_FAN);

    FirstIndex := BeginIndex;

    RenderVertex(BeginIndex);
    RenderVertex(BeginIndex + 1);

    while BeginIndex + 2 < EndIndex do
    begin
      DoNormal(FirstIndex, BeginIndex + 1, BeginIndex + 2);
      RenderVertex(BeginIndex + 2);
      Inc(BeginIndex);
    end;

    glEnd;
  end;
end;

{ TTriangleStripSetRenderer -------------------------------------------------- }

constructor TTriangleStripSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);

  procedure CreateForIndexedTriangleMesh_1;
  var
    SH: TNodeShapeHints;
    ANode: TNodeIndexedTriangleMesh_1;
  begin
    ANode := Geometry as TNodeIndexedTriangleMesh_1;

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

  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
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
    Normals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
    NormalsFree := true;
    NormalsCcw := true;
    NorImplementation := niPerVertexCoordIndexed;
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

procedure TTriangleStripSetRenderer.RenderCoordinate;
begin
  Geometry.MakeCoordRanges(State, @RenderCoordsRange);
end;

procedure TTriangleStripSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    if NorImplementation = niNone then
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      glNormalv(TriangleNormal(GetVertex(V1), GetVertex(V2), GetVertex(V3)));
  end;

var
  NormalOrder: boolean;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
  begin
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
  end;
end;

{ TQuadSetRenderer ------------------------------------------------------- }

constructor TQuadSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  TexCoord := (Geometry as TNodeX3DComposedGeometryNode).TexCoord;
  Color := (Geometry as TNodeX3DComposedGeometryNode).Color;
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
    Normals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
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
end;

procedure TQuadSetRenderer.RenderCoordinate;
var
  I: Integer;
begin
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
begin
  Geometry.MakeCoordRanges(State, @RenderCoordsRange);
end;

{ TIndexedLineSet_1Renderer -------------------------------------------------- }

constructor TIndexedLineSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeIndexedLineSet_1;
begin
  inherited Create(ARenderer);

  Node := Geometry as TNodeIndexedLineSet_1;

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

    Color := NodeILS.Color;
    ColorPerVertex := NodeILS.FdColorPerVertex.Value;
    ColorIndex := NodeILS.FdColorIndex;
  end else
  begin
    Assert(Geometry is TNodeLineSet);
    NodeLS := Geometry as TNodeLineSet;

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
    if State.ParentShape = nil then
    begin
      VRMLNonFatalError('LineSet in VRML >= 2.0 must be specified only inside Shape.geometry (but it''s not)');
      Renderer.SetColor(White3Single);
    end else
    begin
      Material := State.ParentShape.Material;
      if Material <> nil then
        Renderer.SetColor(Material.FdEmissiveColor.Value) else
        Renderer.SetColor(White3Single);
    end;
  end;
end;

function TLineSet_2Renderer.NeedsSetColor: boolean;
begin
  if Color = nil then
    Result := true else
    Result := inherited NeedsSetColor;
end;
