{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Calculating normal vectors for various 3D objects,
  with appropriate smoothing.)

  This is developed for VRML/X3D geometric primitives,
  although some parts are not coupled with VRML/X3D stuff.
  So it can be used in other situations too. }
unit CastleInternalNormals;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, CastleVectors, X3DNodes;

{ Calculate normal vectors for indexed faces, smoothing them according
  to CreaseAngleRad.

  CoordIndex are indexes to Vertices. Indexes < 0 are used to separate
  faces. So this works just like VRML/X3D IndexedFaceSet.coordIndex.

  It's smart and ignores incorrect indexes (outside Vertices range),
  and incorrect triangles in the face (see IndexedPolygonNormal).

  Returns a list of normalized vectors. This has the same Count
  as CoordIndex, and should be accessed in the same way.
  This way you (may) have different normal vector values for each
  vertex on each face, so it's most flexible.
  (For negative indexes in CoordIndex, corresponding value in result
  is undefined.)

  Remember it's your responsibility to free result of this function
  at some point.

  @param(FromCcw Specifies whether we should generate normals
    pointing from CCW (counter-clockwise) or CW.)

  @param(CreaseAngleRad Specifies in radians what is the acceptable
    angle for smoothing adjacent faces. More precisely, we calculate
    for each vertex it's neighbor faces normals. Then we divide these
    faces into groups, such that each group has faces that have normals
    within CreaseAngleRad range, and this group results in one smoothed
    normal. For example, it's possible for a vertex shared by 4 faces
    to be smoothed on first two faces and last two faces separately.

    Note that when creaseAngleRad >= Pi, you wil be better off
    using CreateSmoothNormals. This will work faster, and return shorter
    normals array (so it's also more memory-efficient).)

  @param(Convex Set this to @true if you know the faces are convex.
    This makes calculation faster (but may yield incorrect results
    for concave polygons).) }
function CreateNormals(CoordIndex: TInt32List;
  Vertices: TVector3List;
  CreaseAngleRad: Single;
  const FromCcw, Convex: boolean): TVector3List;

{ Calculate flat per-face normals for indexed faces.

  Note that the result is not a compatible replacement for CreateNormals,
  as it's Count is the number of @italic(faces). For each face, a single
  normal is stored, as this is most sensible compact representation.
  Using something larger would be a waste of memory and time. }
function CreateFlatNormals(coordIndex: TInt32List;
  vertices: TVector3List;
  const FromCcw, Convex: boolean): TVector3List;

{ Calculate always smooth normals per-vertex, for VRML/X3D coordinate-based
  node. We use TAbstractGeometryNode.InternalCoordPolygons for this, so the node class
  must implement it.

  Note that the result is not a compatible replacement for CreateNormals,
  as this generates Coordinates.Count normal vectors in result.
  You should access these normal vectors just like Node.Coordinates,
  i.e. they are indexed by Node.CoordIndex if Node.CoordIndex <> nil.

  If Node.Coordinates is @nil (which means that node is coordinate-based,
  but "coord" field is not present), we return @nil. }
function CreateSmoothNormalsCoordinateNode(
  Node: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  const FromCcw: boolean): TVector3List;

implementation

uses Generics.Collections,
  X3DFields, CastleTriangulate;

type
  TFace = record
    StartIndex: Integer;
    IndicesCount: Integer;
    Normal: TVector3;
  end;
  PFace = ^TFace;

  TFaceList = {$ifdef FPC}specialize{$endif} TStructList<TFace>;

function CreateNormals(CoordIndex: TInt32List;
  Vertices: TVector3List;
  CreaseAngleRad: Single;
  const FromCcw, Convex: boolean): TVector3List;
var
  Faces: TFaceList;
  { For each vertex (this array Count is always Vertices.Count),
    to which faces this vertex belongs? Contains indexes to Faces[] list.

    Although vertex may be more than once on the same face (in case
    of incorrect data, or some concave faces), a face is mentioned
    at most once (for given vertex) in this structure. }
  VerticesFaces: array of TIntegerList;
  NormalsResult: TVector3List absolute Result;
  CosCreaseAngle: Single;

  procedure CalculateFacesAndVerticesFaces;
  var
    ThisFace: PFace;
    I, ThisFaceNum: Integer;
  begin
    I := 0;
    while I < CoordIndex.Count do
    begin
      ThisFaceNum := Faces.Count;
      ThisFace := PFace(Faces.Add);

      ThisFace^.StartIndex := I;
      while (I < CoordIndex.Count) and (CoordIndex[I] >= 0) do
      begin
        { Check that CoordIndex[I] is valid (within Vertices.Count range).
          Note that we cannot remove here wrong indexes from CoordIndex.
          It's tempting, but:
          - Removing them is not so easy. We would have to modify also other
            xxxIndex fields e.g. IndexedFaceSet.texCoordIndex.
          - Our engine generally preserves VRML/X3D data, never auto-correcting
            it (it's a decision that makes various things safer). }
        if (CoordIndex[I] < Vertices.Count) and
           { Make sure to add only the 1st occurrence of a vertex on this face.
             Valid concave faces may specify the same vertex multiple times. }
           (VerticesFaces[CoordIndex[I]].IndexOf(ThisFaceNum) = -1) then
          VerticesFaces[CoordIndex[I]].Add(ThisFaceNum);
        Inc(I);
      end;

      { calculate ThisFace.IndicesCount.
        We completed one face: indexes StartIndex .. i - 1 }
      ThisFace^.IndicesCount := i-ThisFace^.StartIndex;

      { calculate ThisFace.Normal }
      ThisFace^.Normal := IndexedPolygonNormal(
        PInt32Array(CoordIndex.Ptr(ThisFace^.StartIndex)), ThisFace^.IndicesCount,
        PVector3Array(Vertices.L), Vertices.Count,
        Vector3(0, 0, 1), Convex);

      { move to next face (omits the negative index we're standing on) }
      Inc(I);
    end;
  end;

  { For given Face and VertexNum (index to Vertices array),
    set the normal vector in NormalsResult array.
    Vertex must be present at least once on a given face.
    Works OK also in cases when vertex is duplicated (present more than once)
    on a single face. }
  procedure SetNormal(VertexNum: integer; const face: TFace; const Normal: TVector3);
  var
    I: Integer;
    Found: boolean;
  begin
    Found := false;
    for I := Face.StartIndex to Face.StartIndex + Face.IndicesCount - 1 do
      if CoordIndex.L[I] = VertexNum then
      begin
        Found := true; { Found := true, but keep looking in case duplicated }
        NormalsResult.L[I] := Normal;
      end;
    Assert(Found, 'CastleInternalNormals.SetNormal failed, vertex not on face');
  end;

  procedure CalculateVertexNormals(VertexNum: Integer);
  var
    { Initialized to VerticesFaces[VertexNum] }
    ThisVertexFaces: TIntegerList;

    { Can face FaceNum1 be smoothed together with face FaceNum2. }
    function FaceCanBeSmoothedWith(const FaceNum1, FaceNum2: integer): boolean;
    begin
      Result :=
        { I want to check that
            AngleRadBetweenNormals(...) < CreaseAngleRad
          so
            ArcCos(CosAngleRadBetweenNormals(...)) < CreaseAngleRad
          so
            CosAngleBetweenNormals(...) > CosCreaseAngle }
        CosAngleBetweenNormals(
          Faces.L[ThisVertexFaces.L[FaceNum1]].Normal,
          Faces.L[ThisVertexFaces.L[FaceNum2]].Normal) >
          CosCreaseAngle;
    end;

  var
    I, J: Integer;
    Normal: TVector3;
  begin
    ThisVertexFaces := VerticesFaces[VertexNum];
    for I := 0 to ThisVertexFaces.Count - 1 do
    begin
      Normal := Faces.L[ThisVertexFaces[I]].Normal;
      for J := 0 to ThisVertexFaces.Count - 1 do
        if (I <> J) and FaceCanBeSmoothedWith(I, J) then
          Normal := Normal + Faces.L[ThisVertexFaces[J]].Normal;
      Normal := Normal.Normalize;
      SetNormal(VertexNum, Faces.L[ThisVertexFaces[I]], Normal);
    end;
  end;

var
  I: Integer;
begin
  CosCreaseAngle := Cos(CreaseAngleRad);

  SetLength(VerticesFaces, vertices.Count);

  Result := nil;
  Faces := nil;

  try
    try
      for I := 0 to vertices.Count - 1 do
        VerticesFaces[I] := TIntegerList.Create;
      Faces := TFaceList.Create;

      { calculate Faces and VerticesFaces contents }
      CalculateFacesAndVerticesFaces;

      Result := TVector3List.Create;
      Result.Count := CoordIndex.Count;

      { for each vertex, calculate all his normals (on all his faces) }
      for I := 0 to Vertices.Count - 1 do CalculateVertexNormals(I);

      if not FromCcw then Result.Negate;
    finally
      for I := 0 to Vertices.Count - 1 do VerticesFaces[I].Free;
      Faces.Free;
    end;
  except FreeAndNil(Result); raise end;
end;

function CreateFlatNormals(CoordIndex: TInt32List;
  Vertices: TVector3List;
  const FromCcw, Convex: boolean): TVector3List;
var
  I, StartIndex: Integer;
  FaceNumber: Integer;
begin
  { CoordIndex.Count is just a maximum Count, we will shrink it later. }
  Result := TVector3List.Create;
  try
    Result.Count := CoordIndex.Count;
    FaceNumber := 0;

    I := 0;
    while I < CoordIndex.Count do
    begin
      StartIndex := I;
      while (I < CoordIndex.Count) and (CoordIndex.L[I] >= 0) do Inc(I);
      Result.L[FaceNumber] := IndexedPolygonNormal(
        PInt32Array(CoordIndex.Ptr(StartIndex)), I - StartIndex,
        PVector3Array(Vertices.L), Vertices.Count, Vector3(0, 0, 0), Convex);
      Inc(FaceNumber);

      Inc(I);
    end;

    Result.Count := FaceNumber;

    if not FromCcw then Result.Negate;
  except FreeAndNil(Result); raise end;
end;

{ CreateSmoothNormalsCoordinateNode ------------------------------------------ }

type
  TCoordinateNormalsCalculator = class
  public
    Normals: TVector3List;
    CoordIndex: TInt32List;
    Coord: TVector3List;
    Convex: boolean;
    procedure Polygon(const Indexes: array of Cardinal);
  end;

procedure TCoordinateNormalsCalculator.Polygon(
  const Indexes: array of Cardinal);
var
  FaceNormal: TVector3;
  { DirectIndexes is Int32, not Cardinal array, since we cannot
    guarantee that CoordIndex items are >= 0. }
  DirectIndexes: array of Int32;
  I: Integer;
  Index: Int32;
begin
  SetLength(DirectIndexes, Length(Indexes));
  if CoordIndex <> nil then
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := CoordIndex.L[Indexes[I]];
  end else
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := Indexes[I];
  end;

  FaceNormal := IndexedPolygonNormal(
    PInt32Array(DirectIndexes), Length(DirectIndexes),
    PVector3Array(Coord.L), Coord.Count, Vector3(0, 0, 0), Convex);

  for I := 0 to Length(Indexes) - 1 do
  begin
    Index := DirectIndexes[I];
    { Normals count is equal to vertexes count.
      So if Index is incorrect, then we have coordIndex pointing
      to a non-existing vertex index. VRML/X3D code will warn about it
      elsewhere, here just make sure we don't crash. }
    if Index < Normals.Count then
      Normals.L[Index] := Normals.L[Index] + FaceNormal;
  end;
end;

function CreateSmoothNormalsCoordinateNode(
  Node: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  const FromCcw: boolean): TVector3List;
var
  Calculator: TCoordinateNormalsCalculator;
  C: TMFVec3f;
begin
  C := Node.InternalCoordinates(State);

  { Node coordinate-based, but specified with empty coord }
  if C = nil then Exit(nil);

  Result := TVector3List.Create;
  try
    Result.Count := C.Count; { TFPSList initialized everything to 0 }

    Calculator := TCoordinateNormalsCalculator.Create;
    try
      Calculator.Convex := Node.Convex;
      Calculator.Coord := C.Items;
      if Node.CoordIndexField <> nil then
        Calculator.CoordIndex := Node.CoordIndexField.Items else
        Calculator.CoordIndex := nil;
      Calculator.Normals := Result;
      Node.InternalCoordPolygons(State,
        {$ifdef FPC}@{$endif}Calculator.Polygon);
    finally FreeAndNil(Calculator) end;

    Result.Normalize;
    if not FromCcw then Result.Negate;

  except FreeAndNil(Result); raise end;
end;

end.
