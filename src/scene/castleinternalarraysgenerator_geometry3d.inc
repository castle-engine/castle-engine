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

    procedure PrepareIndexesTriangle1(const TriIndices: TVector3Integer);
    procedure PrepareIndexesTriangle2(const TriIndices: TVector3Integer);

    { Add triangle indexes to IndexesFromCoordIndex, TrivialIndexesFromCoordIndex,
      increase IFSNextIndex by 3. }
    procedure AddTriangleIndexes(const T0, T1, T2: UInt32);

    procedure PrepareIndexesCoordsRange1(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
    procedure PrepareIndexesCoordsRange2(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

    procedure GenerateTriangle(const TriIndices: TVector3Integer);
  protected
    { Set these in descendant's constructor. }
    FaceConvex: boolean;

    procedure GenerateCoordinate; override;
    procedure GenerateCoordinateEnd; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    { Auto-generate normals (from NormalsCcw side)
      and set Normals, NorImplementation properties to use them. }
    procedure AutoGenerateNormals(const CreaseAngle: Single);
    procedure PrepareIndexesPrimitives; override;
  public
    class function BumpMappingAllowed: boolean; override;
  end;

  TIndexedFaceSet_1Generator = class(TAbstractIndexedFaceSetGenerator)
  private
    VRML1FrontFaceCcw, VRML1CullFace: boolean;
  protected
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
  end;

  TIndexedFaceSetGenerator = class(TAbstractIndexedFaceSetGenerator)
  public
    constructor Create(AShape: TShape); override;
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

  - TRIANGLES workaround Radeon X1600 Darwin (macOS 10.4.9)
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
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}GenerateCoordsRange);
end;

procedure TAbstractIndexedFaceSetGenerator.AutoGenerateNormals(
  const CreaseAngle: Single);
begin
  { We could use the same NormalsCreaseAngle for all CreaseAngle values,
    so the tests for CreaseAngle are not really required below.
    But this way we're more optimal, as smooth normals allow normals
    per-vertex (so the shape can be rendered with indexes), flat normals
    may allow flat shading (may be faster) and such. }

  if Coord = nil then Exit;

  if CreaseAngle >= Pi then
  begin
    Normals := Shape.NormalsSmooth(NormalsCcw);
    NorImplementation := niPerVertexCoordIndexed;
  end else
  if CreaseAngle <> 0 then
  begin
    Normals := Shape.NormalsCreaseAngle(NormalsCcw, CreaseAngle);
    NorImplementation := niPerVertexNonIndexed;
  end else
  begin
    Normals := Shape.NormalsFlat(NormalsCcw);
    NorImplementation := niPerFace;
  end;
end;

procedure TAbstractIndexedFaceSetGenerator.GenerateCoordinateEnd;
begin
  inherited;
end;

procedure TAbstractIndexedFaceSetGenerator.GenerateTriangle(
  const TriIndices: TVector3Integer);
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
    TriangulateFace(PLongIntArray(CoordIndex.Items.Ptr(BeginIndex)),
      EndIndex - BeginIndex, PVector3Array(Coord.Items.L), Coord.Count,
      {$ifdef FPC}@{$endif}GenerateTriangle, BeginIndex);
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
  const TriIndices: TVector3Integer);
begin
  Inc(IFSIndexesCount, 3);
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesCoordsRange1(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  if not FaceConvex then
  begin
    TriangulateFace(PLongIntArray(CoordIndex.Items.Ptr(BeginIndex)),
      EndIndex - BeginIndex, PVector3Array(Coord.Items.L), Coord.Count,
      {$ifdef FPC}@{$endif}PrepareIndexesTriangle1, BeginIndex);
  end else
  begin
    IFSIndexesCount := IFSIndexesCount + Max(EndIndex - BeginIndex - 2, 0) * 3;
  end;
end;

procedure TAbstractIndexedFaceSetGenerator.AddTriangleIndexes(const T0, T1, T2: UInt32);
begin
  TrivialIndexesFromCoordIndex.L[IFSNextIndex] := T0; IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[T0]; Inc(IFSNextIndex);
  TrivialIndexesFromCoordIndex.L[IFSNextIndex] := T1; IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[T1]; Inc(IFSNextIndex);
  TrivialIndexesFromCoordIndex.L[IFSNextIndex] := T2; IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[T2]; Inc(IFSNextIndex);
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesTriangle2(
  const TriIndices: TVector3Integer);
begin
  AddTriangleIndexes(
    TriIndices.X,
    TriIndices.Y,
    TriIndices.Z
  );
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesCoordsRange2(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I, IFSNextIndexBegin: Integer;
begin
  IFSNextIndexBegin := IFSNextIndex;

  if not FaceConvex then
  begin
    TriangulateFace(PLongIntArray(CoordIndex.Items.Ptr(BeginIndex)),
      EndIndex - BeginIndex, PVector3Array(Coord.Items.L), Coord.Count,
      {$ifdef FPC}@{$endif}PrepareIndexesTriangle2, BeginIndex);
  end else
  begin
    for I := BeginIndex to EndIndex - 3 do
    begin
      AddTriangleIndexes(
        BeginIndex,
        I + 1,
        I + 2
      );
    end;
  end;

  if FacesNeeded then
    { Note that IFSNextIndex may be 0,
      testcase: tests/data/extrusion_empty_spine_concave.x3dv
      (makes integer overflow with Delphi from autotests). }
    for I := IFSNextIndexBegin to Integer(IFSNextIndex) - 1 do
    begin
      Arrays.Faces.L[I].IndexBegin := BeginIndex;
      Arrays.Faces.L[I].IndexEnd := EndIndex;
    end;
end;

procedure TAbstractIndexedFaceSetGenerator.PrepareIndexesPrimitives;
begin
  { calculate IFSIndexesCount by one iteration over coordIndex }
  IFSIndexesCount := 0;
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}PrepareIndexesCoordsRange1);

  if FacesNeeded then
  begin
    Arrays.Faces := TFaceIndexesList.Create;
    Arrays.Faces.Count := IFSIndexesCount;
  end;

  { calculate TrivialIndexesFromCoordIndex, IndexesFromCoordIndex contents: triangulate }
  IndexesFromCoordIndex := TUInt32List.Create;
  IndexesFromCoordIndex.Count := IFSIndexesCount;
  TrivialIndexesFromCoordIndex := TUInt32List.Create;
  TrivialIndexesFromCoordIndex.Count := IFSIndexesCount;
  IFSNextIndex := 0;
  Geometry.InternalMakeCoordRanges(State,
    {$ifdef FPC}@{$endif}PrepareIndexesCoordsRange2);
  Assert(IFSNextIndex = IFSIndexesCount);
end;

{ TIndexedFaceSet_1Generator -------------------------------------------------- }

constructor TIndexedFaceSet_1Generator.Create(AShape: TShape);
var
  SH: TShapeHintsNode_1;
  ANode: TIndexedFaceSetNode_1;
begin
  inherited;

  ANode := Geometry as TIndexedFaceSetNode_1;

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

  FaceConvex := SH.FdFaceType.Value = FACETYPE_CONVEX;
  {$warnings on}

  NormalIndex := ANode.FdNormalIndex;
  Normals := State.VRML1State.Normal.FdVector.Items;
  NormalsCcw := VRML1FrontFaceCcw;
  NorImplementation := NorImplementationFromVRML1Binding(
    State.VRML1State.NormalBinding.FdValue.Value);
  if NorImplementation = niNone then
    AutoGenerateNormals(SH.FdCreaseAngle.Value);
end;

procedure TIndexedFaceSet_1Generator.GenerateCoordinateBegin;
begin
  inherited;
  { Already calculated in constructor, pass to Arrays now }
  Arrays.FrontFaceCcw := VRML1FrontFaceCcw;
  Arrays.CullFace := VRML1CullFace;
end;

{ TIndexedFaceSetGenerator -------------------------------------------------- }

constructor TIndexedFaceSetGenerator.Create(AShape: TShape);
var
  ANode: TIndexedFaceSetNode;
begin
  inherited;

  ANode := Geometry as TIndexedFaceSetNode;

  TexCoordIndex := ANode.FdTexCoordIndex;

  Normals := ANode.InternalNormal;
  TangentsFromNode := ANode.InternalTangent;
  NormalsCcw := ANode.FdCcw.Value;
  if Normals <> nil then
  begin
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

  { calculate ColorXxx fields }
  ColorNode := ANode.Color;
  Color := ANode.InternalColor;
  ColorRGBA := ANode.InternalColorRGBA;
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;
