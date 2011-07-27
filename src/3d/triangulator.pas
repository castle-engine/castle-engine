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

{ @abstract(Triangulating a polygon.) }
unit Triangulator;

interface

uses SysUtils, VectorMath, KambiUtils;

type
  TTriangulatorProc = procedure(const Tri: TVector3Longint) of object;

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
  triangles is the same, and is equal to normal vector of original polygon
  (calculated e.g. by IndexedPolygonNormal).

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
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
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

implementation

{ Implementation idea based on face2tri.C in C++, from mgflib sources. }

procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

  procedure NewTriangle(const p0, p1, p2: Longint);
  begin
    TriangulatorProc(Vector3Longint(
      p0 + AddToIndices,
      p1 + AddToIndices,
      p2 + AddToIndices));
  end;

  function Verts(I: Longint): TVector3Single;
  begin
    if FaceIndices <> nil then
      Result := Vertices(FaceIndices^[I]) else
      Result := Vertices(I);
  end;

  { calculate the most distant vertex from Center. }
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
  ConvexNormal, Center, NN, E1, E2, E3: TVector3Single;
  Corners, Start, MostDistantVertex, I, P0, P1, P2: Integer;
  DistanceSqr: Single;
  Outs: TDynBooleanArray;
  Empty: boolean;

  function NextNotOut(Index: Integer): Integer;
  begin
    Result := Index;
    repeat Result := (Result+1) mod Count until not Outs.Items[Result];
  end;

begin
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

    MostDistantVertex := GetMostDistantVertex(Center);

    { P1 is the most distant vertex, P0 is previous, P2 is next }
    P1 := MostDistantVertex;
    if P1 = 0 then P0 := Count - 1 else P0 := P1 - 1;
    P2 := (P1 + 1) mod Count;

    ConvexNormal := TriangleNormal(Verts(P0), Verts(P1), Verts(P2));

    Corners := Count;
    P0 := -1;

    Outs := TDynBooleanArray.Create(Count);
    try
      Outs.SetAll(false);

      while Corners >= 3 do
      begin
        Start := P0;

        repeat
          P0 := NextNotOut(P0);
          P1 := NextNotOut(P0);
          P2 := NextNotOut(P1);

          if P0 = Start then break;

          NN := TriangleNormal(Verts(P0), Verts(P1), Verts(P2));
          DistanceSqr := PointsDistanceSqr(NN, ConvexNormal);

          E1 := VectorProduct(NN, VectorSubtract(Verts(P0), Verts(P1)));
          E2 := VectorProduct(NN, VectorSubtract(Verts(P1), Verts(P2)));
          E3 := VectorProduct(NN, VectorSubtract(Verts(P2), Verts(P0)));

          Empty := true;

          for I := 0 to Count - 1 do
            if (not Outs.Items[I]) and (I <> P0) and (I <> P1) and (I <> P2) then
              Empty := Empty and not (
                (VectorDotProduct(E1, VectorSubtract(Verts(I), Verts(P0))) <= -SingleEqualityEpsilon) and
                (VectorDotProduct(E2, VectorSubtract(Verts(I), Verts(P1))) <= -SingleEqualityEpsilon) and
                (VectorDotProduct(E3, VectorSubtract(Verts(I), Verts(P2))) <= -SingleEqualityEpsilon)
                );
        until (DistanceSqr <= 1.0) and Empty;

        { TODO --- is this check really not needed?
          Even on invalid graz.mgf.wrl this check is still not needed?
        if p0 = Start then raise Exception.Create('misbuilt polygonal face');}

        NewTriangle(p0, p1, p2);

        Outs.Items[p1] := True;
        Dec(Corners);
      end;
    finally Outs.Free end;
  end;
end;

type
  TVerticesGenerator = class
    Vertices: PArray_Vector3Single;
    function Generate(Index: Integer): TVector3Single;
  end;

function TVerticesGenerator.Generate(Index: Integer): TVector3Single;
begin
  Result := Vertices^[Index];
end;

procedure TriangulateFace(
  FaceIndices: PArray_Longint; Count: Integer;
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
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
      p0 + AddToIndices,
      p1 + AddToIndices,
      p2 + AddToIndices));
  end;

var
  I: Integer;
begin
  for I := 0 to Count - 3 do
    NewTriangle(0, I + 1, I + 2);
end;

end.
