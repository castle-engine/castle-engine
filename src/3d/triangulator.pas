{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Triangulating a polygon.) }
unit Triangulator;

interface

uses SysUtils, VectorMath, CastleUtils;

type
  TTriangulatorProc = procedure (const Tri: TVector3Longint) of object;

{ Triangulate potentially non-convex face.

  FaceIndices[0]..FaceIndices[Count - 1] are indices
  to the Vertices array. They describe the outline of the polygon (face).
  You can pass FaceIndices = @nil, this is understood that indices
  are just 0..Count - 1 (in other words, it's equivalent
  to setting FaceIndices[0] = 0, FaceIndices[1] = 1 etc.).

  For each resulting triangle we will call TriangulatorProc
  with Tri (first param of TriangulatorProc) containing indices
  to FaceIndices[] array. We return indices to FaceIndices
  (not ready vectors from Vertices, not even indices to Vertices[] array)
  to allow the caller to obtain all information about the triangle.
  In the simple case, you can just use Vertices[FaceIndices[Tri[0..2]]]
  to obtain your triangle, in the more sophisticated cases you have
  other options to e.g. extract other vertex information from whatever
  data you have (see e.g. VRML IndexedFaceSet renderer).

  Generated triangles have the same orientation (normal from ccw etc.)
  as original polygon. This also means that if you're sure that
  your polygon is planar (and it should be --- although we handle gracefully
  small deviations from planar, this procedure doesn't actually handle
  arbitrary (dis)located 3D data) then normal vector of all your
  triangles is the same.

  Note that you generally shouldn't use this procedure if you @italic(know)
  that your polygon is convex. Then using this is a waste of time,
  after all convex polygons can be triangulated much easier.
  You can use TriangulateConvexFace in this case, which has
  deliberately very similar interface to this procedure.

  @param(TriangulatorProcData Is just passed unmodified to every
    TriangulatorProc call (as the second parameter). This is standard
    method to pass whatever data to your callback.)

  @param(AddToIndices
    Indexes returned in Tri[0..2] are all incremented by AddToIndices
    (which may also be negative), this is useful if your FaceIndices
    is actually a pointer to the middle of some larger indexes array.
    Just pass 0 if you don't want this.)

  @seeAlso TriangulateConvexFace

  @groupBegin }
procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: PVector3Single; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint); overload;

procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint); overload;
{ @groupEnd }

{ Triangulate convex polygon.

  This performs very easy triangulation. It has deliberately
  similar interface to TriangulateFace, so it can be used as drop-in
  replacement for TriangulateFace, when you know that your face is
  convex.

  Note that it doesn't even need to know FaceIndices or Vertices,
  it's enough to know Count.

  This also guarantees consequent triangles orientation, like TriangulateFace.

  @seeAlso TriangulateFace }
procedure TriangulateConvexFace(Count: Integer;
  TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

{ Triangulate possibly concave polygon. }
function IndexedPolygonNormal(
  Indices: PArray_Longint; IndicesCount: Integer;
  Verts: PVector3Single; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3Single; const Convex: boolean): TVector3Single;

implementation

uses CastleLog;

{ Write some debug messages about triangulation.

  The same symbol name is used in tests/testtriangulator.pas unit,
  so you can just -dVISUALIZE_TRIANGULATION for the compilation of whole program
  (e.g. add it to tests/compile_console.sh) to have all debugging messages. }
{ $define VISUALIZE_TRIANGULATION}

{ Implementation idea based on face2tri.C in C++, from mgflib sources.

  The algorithm works by finding and cutting off "ears" from the polygon.
  "Ear" is a triangle that satisfies:
  1. it has 1 edge inside the polygon and 2 edges along the polygon border and
  2. it can be safely cut off from the polygon (without intersecting any edge).
     Which means that it doesn't contain any other polygon vertex inside.

  When we find an ear, we cut it off, which means
  - we pass the ear triangle to the callback TriangulatorProc,
  - the polygon is shrunk, it has one vertex less: this is done by setting
    Outs (for the corner vertex) to true,
  - and then we repeat the work for the new polygon, smaller by 1 vertex. }

procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

  { Previous and Next in modulo Count (0.. Count - 1) range. }
  function Previous(const I: Integer): Integer;
  begin
    Result := I - 1;
    if Result = -1 then Result += Count;
  end;

  function Next(const I: Integer): Integer;
  begin
    Result := (I + 1) mod Count;
  end;

  procedure NewTriangle(const p0, p1, p2: Longint);
  begin
    TriangulatorProc(Vector3Longint(
      P0 + AddToIndices,
      P1 + AddToIndices,
      P2 + AddToIndices));
  end;

  function Verts(I: Longint): TVector3Single;
  begin
    if FaceIndices <> nil then
      Result := Vertices(FaceIndices^[I]) else
      Result := Vertices(I);
  end;

  { Calculate the most distant vertex from Center. }
  function GetMostDistantVertex(const Center: TVector3Single): Integer;
  var
    MaxLen, D: Single;
    I: Integer;
  begin
    Result := 0;
    MaxLen := PointsDistanceSqr(Center, Verts(0));
    for I := 1 to Count - 1 do
    begin
      D := PointsDistanceSqr(Center, Verts(I));
      if D > MaxLen then
      begin
        MaxLen := D;
        Result := I;
      end;
    end;
  end;

var
  { Which vertexes are "out" (removed from the polygon).
    Initially none (everything is false).
    When we find an ear triangle, it's corner vertex is removed
    from the polygon by setting corresponding item of this array to true. }
  Outs: TBooleanList;

  { Increase Index, until a value with Outs[Result]=false is found. }
  function NextNotOut(Index: Integer): Integer;
  begin
    Result := Index;
    repeat Result := Next(Result) until not Outs[Result];
  end;

var
  PolygonNormal, Center, EarNormal, E1, E2, E3: TVector3Single;
  Corners, Start, I, P0, P1, P2: Integer;
  DistanceSqr: Single;
  Empty: boolean;
  EpsilonForEmptyCheck: Single;
begin
  EpsilonForEmptyCheck := SingleEqualityEpsilon;

  if Count = 3 then
    { For Count = 3 this is trivial, do it fast. }
    NewTriangle(0, 1, 2) else
  if Count > 3 then
  begin
    { calculate Center := average of all vertexes }
    Center := ZeroVector3Single;
    for I := 0 to Count - 1 do
      Center += Verts(I);
    Center /= Count;

    { P1 is the most distant vertex, P0 is previous, P2 is next.
      We calculate them only for the sake of calculating PolygonNormal
      (they do not determine triangulation in any other way). }
    P1 := GetMostDistantVertex(Center);
    { P0 := previous from P1, with different value. }
    P0 := P1;
    repeat
      P0 := Previous(P0);
    until (P0 = P1) or not VectorsEqual(Verts(P0), Verts(P1));
    if P0 = P1 then
    begin
      if Log then WritelnLog('Triangulator', 'All vertexes of given polygon are equal. So polygon doesn''t contain any non-empty triangles.');
      Exit;
    end;
    { P2 := next from P1, resulting in valid triangle P0-P1-P2. }
    P2 := P1;
    repeat
      P2 := Next(P2);
      PolygonNormal := TriangleDir(Verts(P0), Verts(P1), Verts(P2));
      { note: no need to check for VectorsEqual(Verts(P2), Verts(P1)) anywhere,
        because if PolygonNormal is non-zero then they had to be different. }
    until (P2 = P1) or not ZeroVector(PolygonNormal);
    if P2 = P1 then
    begin
      if Log then WritelnLog('Triangulator', 'All vertexes of given polygon are collinear. So polygon doesn''t contain any non-empty triangles.');
      Exit;
    end;

    Assert(not ZeroVector(PolygonNormal));
    NormalizeTo1st(PolygonNormal);

    {$ifdef VISUALIZE_TRIANGULATION}
    Writeln(Format('Most distant vertex is %d. The triangle for PolygonNormal is %d - %d - %d.',
      [P1, P0, P1, P2]));
    {$endif VISUALIZE_TRIANGULATION}

    Corners := Count; { Corners = always "how many Outs are false" }
    { This initial P0 value is a "border", used to prevent an infinite loop
      when we cannot find good ear triangle (which is always possible due
      to lack of floating-point precision).
      It will always be increased by first "P0 := NextNotOut(P0);".
      It must be a valid vertex index, otherwise condition "P0 = Start"
      could never occur and we would loop forever (testcase:
      change EpsilonForEmptyCheck to 0, and run on demo_models/x3d/concave.x3dv). }
    P0 := Count - 1;
    Outs := TBooleanList.Create;
    try
      Outs.Count := Count; { TFPGList initialized everything to false }

      while Corners >= 3 do
      begin
        Start := P0;

        { remove duplicate vertices }
        P0 := NextNotOut(Start);
        P1 := P0;
        repeat
          P2 := NextNotOut(P1);
          if VectorsEqual(Verts(P1), Verts(P2)) then
          begin
             Outs[P2] := true;
             Dec(Corners);
          end;
          P1 := P2;
        until P1 = P0;
        if Corners < 3 then Break;  { TODO: Really? Will breaking here generate weird triangle? }

        { find next ear triangle }
        repeat
          { increase P0. Set P1 and P2 to vertexes following P0.
            We will now consider triangle P0-P1-P2 to be removed,
            where P1 is the corner to be cut off. }
          P0 := NextNotOut(P0);
          P1 := NextNotOut(P0);
          P2 := NextNotOut(P1);

          { If P0 returned back to Start value,
            then we considered every possible corner triangle and it cannot
            be cut off. IOW, we cannot find any ear triangle,
            so we cannot triangulate this polygon correctly.
            This should happen only because of floating-point inaccuracy,
            because every polygon (with >= 4 vertexes) should have at least
            2 valid "ears" to cut off. }
          if P0 = Start then
          begin
            if Log then WritelnLog('Triangulator', 'Impossible to find an "ear" to cut off, this concave polygon cannot be triangulated. This should be caused only by floating-point inaccuracy (you use some incredibly huge and/or tiny values), or self-intersecting polygon, otherwise report a bug.');
            Break;
          end;

          EarNormal := TriangleDir(Verts(P0), Verts(P1), Verts(P2));
          if ZeroVector(EarNormal) then
          begin
            if Log then WritelnLog('Triangulator', 'Triangle inside polygon is degenerated, (TODO) rejecting.');
//            Break; // TODO: do something here. It seems that break would be Ok, just cut off P1?
          end;
          NormalizeTo1st(EarNormal);

          { DistanceSqr is used to check that P0-P1-P2 has roughly the same
            orientation as whole polygon, not reverted. }
          DistanceSqr := PointsDistanceSqr(EarNormal, PolygonNormal);

          { vectors orthogonal to triangle edges going *outside* from the triangle }
          E1 := VectorProduct(EarNormal, Verts(P0) - Verts(P1));
          E2 := VectorProduct(EarNormal, Verts(P1) - Verts(P2));
          E3 := VectorProduct(EarNormal, Verts(P2) - Verts(P0));

          Empty := true;
          for I := 0 to Count - 1 do
            { if we can find a vertex that is
              - part of the polygon (not "out" yet)
              - different than P0, P1, P2
              - inside P0-P1-P2 triangle (this is checked by looking at angle
                between E? and vector to given vertex, value > 90 degrees means
                vertex is inside the triangle (for given edge))
              then the considered triangle is not empty, and it cannot be removed
              as an ear triangle. }
            if (not Outs[I]) and
               (I <> P0) and
               (I <> P1) and
               (I <> P2) and
               (VectorDotProduct(E1, Verts(I) - Verts(P0)) <= -EpsilonForEmptyCheck) and
               (VectorDotProduct(E2, Verts(I) - Verts(P1)) <= -EpsilonForEmptyCheck) and
               (VectorDotProduct(E3, Verts(I) - Verts(P2)) <= -EpsilonForEmptyCheck) then
            begin
              Empty := false;
              Break;
            end;
        until (DistanceSqr <= 1.0) and Empty;

        { ear triangle found, cut if off now }
        NewTriangle(P0, P1, P2);
        Outs[P1] := true;
        Dec(Corners);
      end;
    finally Outs.Free end;
  end;
end;

type
  TVerticesGenerator = class
    Vertices: PVector3Single;
    function Generate(Index: Integer): TVector3Single;
  end;

function TVerticesGenerator.Generate(Index: Integer): TVector3Single;
begin
  Result := Vertices[Index];
end;

procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: PVector3Single; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);
var
  G: TVerticesGenerator;
begin
  G := TVerticesGenerator.Create;
  try
    G.Vertices := Vertices;
    TriangulateFace(FaceIndices, Count,
      @G.Generate, TriangulatorProc, AddToIndices);
  finally FreeAndNil(G); end;
end;

procedure TriangulateConvexFace(Count: Integer;
  TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

  procedure NewTriangle(const p0, p1, p2: Longint);
  begin
    TriangulatorProc(Vector3Longint(
      P0 + AddToIndices,
      P1 + AddToIndices,
      P2 + AddToIndices));
  end;

var
  I: Integer;
begin
  for I := 0 to Count - 3 do
    NewTriangle(0, I + 1, I + 2);
end;

type
  TConcaveTriangulator = class
    Indices: PArray_Longint;
    Verts: PVector3Single;
    VertsCount: Integer;
    Normal: TVector3Single;
    procedure HandleTriangle(const Tri: TVector3Longint);
  end;

procedure TConcaveTriangulator.HandleTriangle(const Tri: TVector3Longint);
var
  I0, I1, I2: Integer;
begin
  I0 := Indices^[Tri[0]];
  I1 := Indices^[Tri[1]];
  I2 := Indices^[Tri[2]];
  if (I0 < VertsCount) and
     (I1 < VertsCount) and
     (I2 < VertsCount) then
    Normal += TriangleNormal(Verts[I0], Verts[I1], Verts[I2]);
end;

function IndexedConcavePolygonNormal(
  Indices: PArray_Longint; IndicesCount: Integer;
  Verts: PVector3Single; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3Single): TVector3Single;
var
  T: TConcaveTriangulator;
begin
  T := TConcaveTriangulator.Create;
  try
    T.Indices := Indices;
    T.Verts := Verts;
    T.VertsCount := VertsCount;
    TriangulateFace(Indices, IndicesCount, Verts, @T.HandleTriangle, 0);
    Result := T.Normal;
    if ZeroVector(Result) then
      Result := ResultForIncorrectPoly else
      NormalizeTo1st(Result);
  finally FreeAndNil(T) end;
end;

function IndexedPolygonNormal(
  Indices: PArray_Longint; IndicesCount: Integer;
  Verts: PVector3Single; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3Single; const Convex: boolean): TVector3Single;
begin
  if Convex then
    Result := IndexedConvexPolygonNormal (Indices, IndicesCount, Verts, VertsCount, ResultForIncorrectPoly) else
    Result := IndexedConcavePolygonNormal(Indices, IndicesCount, Verts, VertsCount, ResultForIncorrectPoly);
end;

end.
