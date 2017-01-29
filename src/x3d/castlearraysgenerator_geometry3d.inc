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

{ TVRMLArraysGenerator descendants implementing nodes in X3D "Geometry3D"
  component. }

type
  { Handling IndexedFaceSet for VRML <= 1.0 and >= 2.0.

    Note that this cannot deal with NorImplementation = niNone,
    you're expected to set NorImplementation to non-none in constructor.
    A simplest way to do this is to use AutoGenerateNormals. }
  TAbstractIndexedFaceSetGenerator = class(TAbstractCompleteGenerator)
  private
    IFSIndexesCount: Cardinal;
    IFSNextIndex: Cardinal;

    procedure PrepareIndexesTriangle1(const TriIndices: TVector3Longint);
    procedure PrepareIndexesTriangle2(const TriIndices: TVector3Longint);

    procedure PrepareIndexesCoordsRange1(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
    procedure PrepareIndexesCoordsRange2(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

    procedure GenerateTriangle(const TriIndices: TVector3Longint);
  protected
    { Set these in descendant's constructor. }
    FaceConvex: boolean;

    procedure GenerateCoordinate; override;
    procedure GenerateCoordinateEnd; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    { Auto-generate normals and set Normals, NormalsCcw, NorImplementation
      properties to use them. }
    procedure AutoGenerateNormals(const CreaseAngle: Single);
    procedure PrepareIndexesPrimitives; override;
  public
    class function BumpMappingAllowed: boolean; override;
  end;

  TIndexedFaceSet_1Generator = class(TAbstractIndexedFaceSetGenerator)
  private
    VRML1FrontFaceCcw, VRML1CullBackFaces: boolean;
  protected
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape; AOverTriangulate: boolean); override;
  end;

  TIndexedFaceSetGenerator = class(TAbstractIndexedFaceSetGenerator)
  public
    constructor Create(AShape: TShape; AOverTriangulate: boolean); override;
  end;

{ TAbstractIndexedFaceSetGenerator ------------------------------------------- }

class function TAbstractIndexedFaceSetGenerator.BumpMappingAllowed: boolean;
begin
  Result := true;
end;

{ TIndexedFaceSetGenerator makes triangles, GL_TRIANGLES in OpenGL.
  Making one TRIANGLES primitive is fast, and allows us to do everything
  we want.

  Old notes: I was experimenting also with making TRIANGLE_FAN (or POLYGON)
  per face.

  Advantages of TRIANGLE_FAN:

  - TRIANGLE_FAN *may* increase speed, because vertices
    are more shared and specified less times. For TRIANGLES, we waste 2 vertex
    calls for every vertex (above 3) for each face.

  - TRIANGLE_FAN workarounds NVidia GeForce 5200 bug for material
    binding PER_FACE_INDEXED, see
    demo_models/vrml_1/material_per_face_problematic_on_nvidia_gf_5200.wrl
    Although the problem disappeared by itself after using GL_COLOR_MATERIAL
    for VRML 1.0 per-face/vertex materials.

  Avantages of TRIANGLES:

  - TRIANGLE_FAN is highly unoptimal on newer GPUs (Radeon on chantal).
    The problem is you have to call glEnd / glBegin again
    a lot, and this hurts pipeline performance very badly. And the "sharing"
    argument for TRIANGLE_FAN is weak, as newer GPUs cache vertex data
    by index anyway.

    Tests on Radeon X1600 (chantal, fglrx) show
    TRIANGLES    FPS: 165/155
    TRIANGLE_FAN FPS: 60/55
    Clearly, TRIANGLES win.

    Although tests on NVidia (kocury, GeForce FX 5200) don't show such
    drastic difference.

  - TRIANGLES workaround Radeon X1600 Darwin (Mac OS X 10.4.9)
    OpenGL bugs. (With Linux fglrx on the same hardware there is no bug.)
    It seems not able to handle GL_TRIANGLE_FAN properly,
    which is visible on "The Gate" and
    "Cages" levels of "The Castle": some triangles on these
    levels (on "The Gate", it concerns that back wall and gate
    geometry) seem to randomly disappear.

  This is a shortcut of even longer old comments... For the full story,
  see revision 6432 in SVN repo,
  and search for TryRenderingFaceAsFan comments.
}

procedure TAbstractIndexedFaceSetGenerator.GenerateCoordinate;
begin
  Geometry.MakeCoordRanges(State, @GenerateCoordsRange);
end;

procedure TAbstractIndexedFaceSetGenerator.AutoGenerateNormals(const CreaseAngle: Single);
begin
  { We could use the same NormalsCreaseAngle for all CreaseAngle values,
    so the tests for CreaseAngle are not really required below.
    But this way we're more optimal, as smooth normals allow normals
    per-vertex (so the shape can be rendered with indexes), flat normals
    may allow flat shading (may be faster) and such. }

  if Coord = nil then Exit;

  if CreaseAngle >= Pi then
  begin
    Normals := Shape.NormalsSmooth(OverTriangulate);
    NorImplementation := niPerVertexCoordIndexed;
  end else
  if CreaseAngle <> 0 then
  begin
    Normals := Shape.NormalsCreaseAngle(OverTriangulate, CreaseAngle);
    NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    Normals := Shape.NormalsFlat(OverTriangulate);
    NorImplementation := niPerFace;
  end;
  NormalsCcw := true; { always generated from CCW }
end;

procedure TAbstractIndexedFaceSetGenerator.GenerateCoordinateEnd;
begin
  inherited;
end;

procedure TAbstractIndexedFaceSetGenerator.GenerateTriangle(
  const TriIndices: TVector3Longint);
begin
  GenerateVertex(TriIndices[0]);
  GenerateVertex(TriIndices[1]);
  GenerateVertex(TriIndices[2]);
end;

procedure TAbstractIndexedFaceSetGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  inherited;

  if BeginIndex + 2 < EndIndex then
    CalculateTangentVectors(BeginIndex, BeginIndex + 1, BeginIndex + 2);

  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.List^[BeginIndex]),
      EndIndex - BeginIndex, PVector3Single(Coord.Items.List), Coord.Count,
      @GenerateTriangle, BeginIndex);
  end else
  begin
    (* Alternative version:
    TriangulateConvexFace(EndIndex - BeginIndex, @GenerateVertex, BeginIndex); *)

    for I := BeginIndex to EndIndex - 3 do
    begin
      GenerateVertex(BeginIndex);
      GenerateVertex(I + 1);
      GenerateVertex(I + 2);
    end;
  end;
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesTriangle1(
  const TriIndices: TVector3Longint);
begin
  IFSIndexesCount += 3;
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesCoordsRange1(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.List^[BeginIndex]),
      EndIndex - BeginIndex, PVector3Single(Coord.Items.List), Coord.Count,
      @PrepareIndexesTriangle1, BeginIndex);
  end else
  begin
    IFSIndexesCount += Max(EndIndex - BeginIndex - 2, 0) * 3;
  end;
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesTriangle2(
  const TriIndices: TVector3Longint);
begin
  IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[TriIndices[0]]; Inc(IFSNextIndex);
  IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[TriIndices[1]]; Inc(IFSNextIndex);
  IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[TriIndices[2]]; Inc(IFSNextIndex);
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesCoordsRange2(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I, IFSNextIndexBegin: Integer;
begin
  IFSNextIndexBegin := IFSNextIndex;

  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.List^[BeginIndex]),
      EndIndex - BeginIndex, PVector3Single(Coord.Items.List), Coord.Count,
      @PrepareIndexesTriangle2, BeginIndex);
  end else
  begin
    for I := BeginIndex to EndIndex - 3 do
    begin
      IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[BeginIndex]; Inc(IFSNextIndex);
      IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[I + 1];      Inc(IFSNextIndex);
      IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[I + 2];      Inc(IFSNextIndex);
    end;
  end;

  if FacesNeeded then
    for I := IFSNextIndexBegin to IFSNextIndex - 1 do
    begin
      Arrays.Faces.L[I].IndexBegin := BeginIndex;
      Arrays.Faces.L[I].IndexEnd := EndIndex;
    end;
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesPrimitives;
begin
  { calculate IFSIndexesCount by one iteration over coordIndex }
  IFSIndexesCount := 0;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange1);

  if FacesNeeded then
  begin
    Arrays.Faces := TFaceIndexesList.Create;
    Arrays.Faces.Count := IFSIndexesCount;
  end;

  { calculate IndexesFromCoordIndex contents: triangulate }
  IndexesFromCoordIndex := TGeometryIndexList.Create;
  IndexesFromCoordIndex.Count := IFSIndexesCount;
  IFSNextIndex := 0;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange2);
  Assert(IFSNextIndex = IFSIndexesCount);
end;

{ TIndexedFaceSet_1Generator -------------------------------------------------- }

constructor TIndexedFaceSet_1Generator.Create(AShape: TShape; AOverTriangulate: boolean);
var
  SH: TShapeHintsNode_1;
  ANode: TIndexedFaceSetNode_1;
begin
  inherited;

  ANode := Geometry as TIndexedFaceSetNode_1;

  TexCoordIndex := ANode.FdTextureCoordIndex;

  MaterialIndex := ANode.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
  UpdateMat1Implementation;

  SH := State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  VRML1FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

  VRML1CullBackFaces :=
    (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID);

  FaceConvex := SH.FdFaceType.Value = FACETYPE_CONVEX;

  NormalIndex := ANode.FdNormalIndex;
  Normals := State.LastNodes.Normal.FdVector.Items;
  NormalsCcw := VRML1FrontFaceCcw;
  NorImplementation := NorImplementationFromVRML1Binding(
    State.LastNodes.NormalBinding.FdValue.Value);
  if NorImplementation = niNone then
    AutoGenerateNormals(SH.FdCreaseAngle.Value);

  RadianceTransfer := ANode.FdRadianceTransfer.Items;
end;

procedure TIndexedFaceSet_1Generator.GenerateCoordinateBegin;
begin
  inherited;
  { Already calculated in constructor, pass to Arrays now }
  Arrays.FrontFaceCcw := VRML1FrontFaceCcw;
  Arrays.CullBackFaces := VRML1CullBackFaces;
end;

{ TIndexedFaceSetGenerator -------------------------------------------------- }

constructor TIndexedFaceSetGenerator.Create(AShape: TShape; AOverTriangulate: boolean);
var
  ANode: TIndexedFaceSetNode;
begin
  inherited;

  ANode := Geometry as TIndexedFaceSetNode;

  TexCoordIndex := ANode.FdTexCoordIndex;

  Normals := ANode.NormalItems;
  if Normals <> nil then
  begin
    NormalsCcw := ANode.FdCcw.Value;
    NormalIndex := ANode.FdNormalIndex;
    if ANode.FdNormalPerVertex.Value then
    begin
      if NormalIndex.Count > 0 then
        NorImplementation := niPerVertexNormalIndexed else
        NorImplementation := niPerVertexCoordIndexed;
    end else
    begin
      if NormalIndex.Count > 0 then
        NorImplementation := niPerFaceNormalIndexed else
        NorImplementation := niPerFace;
    end;
  end else
    AutoGenerateNormals(ANode.FdCreaseAngle.Value);

  FaceConvex := ANode.FdConvex.Value;

  { calculate Color, ColorPerVertex, ColorIndex fields }
  Color := ANode.Color;
  ColorRGBA := ANode.ColorRGBA;
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;
