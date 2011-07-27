{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Calculating normal vectors for various 3D objects,
  with appropriate smoothing.)

  This is developed for VRML/X3D geometric primitives,
  although some parts are not coupled with VRML/X3D stuff.
  So it can be used in other situations too. }
unit NormalsCalculator;

interface

uses SysUtils, KambiUtils, VectorMath, VRMLNodes;

{ Calculate normal vectors for indexed faces, smoothing them according
  to CreaseAngleRad.

  CoordIndex are indexes to Vertices. Indexes < 0 are used to separate
  faces. So this works just like VRML/X3D IndexedFaceSet.coordIndex.

  It's smart and ignores incorrect indexes (outside Vertices range),
  and incorrect faces triangles (see IndexedPolygonNormal).
  It's guaranteed to work Ok for convex faces, although for non-convex faces
  results are also acceptable (as results of IndexedPolygonNormal
  should be acceptable for even non-convex faces).

  Returns a list of normalized vectors. This has the same length
  as CoordIndex, and should be accessed in the same way.
  This way you (may) have different normal vector values for each
  vertex on each face, so it's most flexible.
  (For negative indexes in CoordIndex, corresponding value in result
  is undefined.)

  Remember it's your responsibility to free result of this function
  at some point.

  @param(FromCCW Specifies whether we should generate normals
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
    normals array (so it's also more memory-efficient).) }
function CreateNormals(CoordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  creaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate flat per-face normals for indexed faces.

  Note that the result is not a compatible replacement for CreateNormals,
  as it's length is the number of @italic(faces). For each face, a single
  normal is stored, as this is most sensible compact representation.
  Using something larger would be a waste of memory and time. }
function CreateFlatNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate always smooth normals per-vertex, for VRML coordinate-based
  node. We use TVRMLGeometryNode.CoordPolygons for this, so the node class
  must implement it.

  Note that the result is not a compatible replacement for CreateNormals,
  as this generates Coordinates.Count normal vectors in result.
  You should access these normal vectors just like Node.Coordinates,
  i.e. they are indexed by Node.CoordIndex if Node.CoordIndex <> nil.

  If Node.Coordinates is @nil (which means that node is coordinate-based,
  but "coord" field is not present), we return @nil. }
function CreateSmoothNormalsCoordinateNode(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  FromCCW: boolean): TDynVector3SingleArray;

implementation

uses VRMLFields;

{$define read_interface}
{$define read_implementation}

type
  TFace = record
    StartIndex: Integer;
    IndicesCount: Integer;
    Normal: TVector3Single;
  end;
  PFace = ^TFace;

  TDynArrayItem_1 = TFace;
  PDynArrayItem_1 = PFace;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  type TDynFaceArray = TDynArray_1;

function CreateNormals(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  CreaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;
var
  Faces: TDynFaceArray;
  { For each vertex (this array length is always Vertices.Count),
    to which faces this vertex belongs? Contains indexes to Faces[] list.

    Although vertex may be more than once on the same face (in case
    of incorrect data, or some concave faces), a face is mentioned
    at most once (for given vertex) in this structure. }
  VerticesFaces: array of TDynIntegerArray;
  NormalsResult: TDynVector3SingleArray absolute Result;
  CosCreaseAngle: Single;

  procedure CalculateFacesAndVerticesFaces;
  var
    ThisFace: PFace;
    I, ThisFaceNum: Integer;
  begin
    I := 0;
    while I < CoordIndex.Count do
    begin
      ThisFaceNum := Faces.Length;
      ThisFace := Faces.Add;

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
        @(CoordIndex.Items[ThisFace^.StartIndex]), ThisFace^.IndicesCount,
        Vertices.ItemsArray, Vertices.Count,
        Vector3Single(0, 0, 1));

      { move to next face (omits the negative index we're standing on) }
      Inc(I);
    end;
  end;

  { For given Face and VertexNum (index to Vertices array),
    set the normal vector in NormalsResult array.
    Vertex must be present at least once on a given face.
    Works OK also in cases when vertex is duplicated (present more than once)
    on a single face. }
  procedure SetNormal(VertexNum: integer; const face: TFace; const Normal: TVector3Single);
  var
    I: Integer;
    Found: boolean;
  begin
    Found := false;
    for I := Face.StartIndex to Face.StartIndex + Face.IndicesCount - 1 do
      if CoordIndex.Items[I] = VertexNum then
      begin
        Found := true; { Found := true, but keep looking in case duplicated }
        NormalsResult.Items[I] := Normal;
      end;
    Assert(Found, 'NormalsCalculator.SetNormal failed, vertex not on face');
  end;

  procedure CalculateVertexNormals(VertexNum: Integer);
  var
    { Initialized to VerticesFaces[VertexNum] }
    ThisVertexFaces: TDynIntegerArray;

    { Can face FaceNum may be smoothed together with all faces in FaceNums.
      This is the moment when CreaseAngleRad comes into play.
      FaceNum and FaceNums[] are indexes to ThisVertexFaces array. }
    function FaceCanBeSmoothedWithFaces(FaceNum: integer;
      FaceNums: TDynIntegerArray): boolean;
    var
      I: integer;
    begin
      for I := 0 to FaceNums.Count - 1 do
        { I want to check that
            AngleRadBetweenNormals(...) >= CreaseAngleRad
          so
            ArcCos(CosAngleRadBetweenNormals(...)) >= CreaseAngleRad
          so
            CosAngleBetweenNormals(...) < CosCreaseAngle }
        if CosAngleBetweenNormals(
          Faces.Items[ThisVertexFaces.Items[FaceNum]].Normal,
          Faces.Items[ThisVertexFaces.Items[FaceNums[i]]].Normal) <
          CosCreaseAngle then
          Exit(false);
      Result := true;
    end;

  var
    I, J: Integer;
    { Current face group that shares a common normal vector on this vertex. }
    SmoothFaces: TDynIntegerArray;
    { Did we store normal vector for given face (and this vertex VertexNum) }
    HandledFaces: TDynBooleanArray;
    Normal: TVector3Single;
  begin
    ThisVertexFaces := VerticesFaces[VertexNum];

    SmoothFaces := nil;
    HandledFaces := nil;
    try
      HandledFaces := TDynBooleanArray.Create(ThisVertexFaces.Count);
      HandledFaces.SetAll(false);
      SmoothFaces := TDynIntegerArray.Create;

      for I := 0 to ThisVertexFaces.Count - 1 do
        if not HandledFaces[I] then
        begin
          { calculate SmoothFaces }
          SmoothFaces.SetLength(1);
          SmoothFaces[0] := i;

          for J := I + 1 to ThisVertexFaces.Count - 1 do
            if (not HandledFaces[j]) and FaceCanBeSmoothedWithFaces(J, SmoothFaces) then
              SmoothFaces.Add(J);

          { handle faces in SmoothFaces }
          FillChar(Normal, SizeOf(Normal), 0);
          for J := 0 to SmoothFaces.Count - 1 do
          begin
            HandledFaces[SmoothFaces[J]] := true;
            VectorAddTo1st(Normal, faces.Items[ThisVertexFaces[SmoothFaces[J]]].Normal);
          end;
          NormalizeTo1st(Normal);

          { use calculated normal vector }
          for J := 0 to SmoothFaces.Count - 1 do
            SetNormal(VertexNum, Faces.Items[ThisVertexFaces[SmoothFaces[J]]], Normal);
        end;
    finally
      SmoothFaces.Free;
      HandledFaces.Free;
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
        VerticesFaces[I] := TDynIntegerArray.Create;
      Faces := TDynFaceArray.Create;

      { calculate Faces and VerticesFaces contents }
      CalculateFacesAndVerticesFaces;

      Result := TDynVector3SingleArray.Create(CoordIndex.Length);

      { for each vertex, calculate all his normals (on all his faces) }
      for I := 0 to Vertices.Count - 1 do CalculateVertexNormals(I);

      if not FromCCW then Result.Negate;
    finally
      for I := 0 to Vertices.Count - 1 do VerticesFaces[I].Free;
      Faces.Free;
    end;
  except FreeAndNil(Result); raise end;
end;

function CreateFlatNormals(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray; FromCCW: boolean): TDynVector3SingleArray;
var
  I, StartIndex: Integer;
  FaceNumber: Integer;
begin
  { CoordIndex.Length is just a maximum length, we will shrink it later. }
  Result := TDynVector3SingleArray.Create(CoordIndex.Length);
  try
    FaceNumber := 0;

    I := 0;
    while I < CoordIndex.Count do
    begin
      StartIndex := I;
      while (I < CoordIndex.Count) and (CoordIndex.Items[I] >= 0) do Inc(I);
      Result.Items[FaceNumber] := IndexedPolygonNormal(
        @(CoordIndex.Items[StartIndex]),
        I - StartIndex,
        Vertices.ItemsArray, Vertices.Count,
        Vector3Single(0, 0, 0));
      Inc(FaceNumber);

      Inc(I);
    end;

    Result.Length := FaceNumber;

    if not FromCCW then Result.Negate;
  except FreeAndNil(Result); raise end;
end;

{ CreateSmoothNormalsCoordinateNode ------------------------------------------ }

type
  TCoordinateNormalsCalculator = class
  public
    Normals: TDynVector3SingleArray;
    CoordIndex: TDynLongIntArray;
    Coord: TDynVector3SingleArray;

    procedure Polygon(const Indexes: array of Cardinal);
  end;

procedure TCoordinateNormalsCalculator.Polygon(
  const Indexes: array of Cardinal);
var
  FaceNormal: TVector3Single;
  { DirectIndexes is LongInt, not Cardinal array, since we cannot
    guarantee that CoordIndex items are >= 0. }
  DirectIndexes: array of LongInt;
  I: Integer;
begin
  SetLength(DirectIndexes, Length(Indexes));
  if CoordIndex <> nil then
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := CoordIndex.Items[Indexes[I]];
  end else
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := Indexes[I];
  end;

  FaceNormal := IndexedPolygonNormal(
    PArray_LongInt(DirectIndexes), Length(DirectIndexes),
    Coord.ItemsArray, Coord.Count,
    Vector3Single(0, 0, 0));

  for I := 0 to Length(Indexes) - 1 do
    VectorAddTo1st(Normals.Items[DirectIndexes[I]], FaceNormal);
end;

function CreateSmoothNormalsCoordinateNode(
  Node: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  FromCCW: boolean): TDynVector3SingleArray;
var
  Calculator: TCoordinateNormalsCalculator;
  C: TMFVec3f;
begin
  C := Node.Coordinates(State);

  { Node coordinate-based, but specified with empty coord }
  if C = nil then Exit(nil);

  Result := TDynVector3SingleArray.Create(C.Count);
  try
    Result.FillChar(0);

    Calculator := TCoordinateNormalsCalculator.Create;
    try
      Calculator.Coord := C.Items;
      if Node.CoordIndex <> nil then
        Calculator.CoordIndex := Node.CoordIndex.Items else
        Calculator.CoordIndex := nil;
      Calculator.Normals := Result;
      Node.CoordPolygons(State, @Calculator.Polygon);
    finally FreeAndNil(Calculator) end;

    Result.Normalize;
    if not FromCCW then Result.Negate;

  except FreeAndNil(Result); raise end;
end;

end.
