{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Triangulating a polygon.) }
unit CastleTriangulate;

{$I castleconf.inc}

interface

uses SysUtils, CastleVectors, CastleUtils, CastleTriangles;

type
  TTriangulatorProc = procedure (const Tri: TVector3Integer) of object;

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
  data you have (see e.g. VRML/X3D IndexedFaceSet renderer).

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
  FaceIndices: PLongintArray; Count: Integer;
  Vertices: PVector3Array; VerticesCount: Integer;
  TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint); overload;

procedure TriangulateFace(
  FaceIndices: PLongintArray; Count: Integer;
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

{ Calculate normal vector of possibly concave polygon. }
function IndexedPolygonNormal(
  Indices: PLongintArray; IndicesCount: Integer;
  Vertices: PVector3Array; const VerticesCount: Integer;
  const ResultForIncorrectPoly: TVector3; const Convex: boolean): TVector3;

var
  { Write to Log a @italic(lot) of comments how the triangulation goes.
    Useful for examples/visualize_triangulation/ . }
  LogTriangulation: boolean;

implementation

uses Math,
  CastleLog, CastleStringUtils;

{ Do additional operations (normalize some vectors etc.)
  that in theory should improve numerical stability of this algorithm.
  These operations do not seem to make any impact in practice in our tests.
  Define this if you see problems, and suspect they may be solved by this
  (and if you find a testcase that proves that this is needed, please report,
  so we will make this working always!). }
{ $define TRIANGULATION_EXTRA_STABILITY}

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
  FaceIndices: PLongintArray; Count: Integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

  { Previous and Next in modulo Count (0.. Count - 1) range. }
  function Previous(const I: Integer): Integer;
  begin
    Result := I - 1;
    if Result = -1 then
      Result := Result + Count;
  end;

  function Next(const I: Integer): Integer;
  begin
    Result := (I + 1) mod Count;
  end;

  procedure NewTriangle(const p0, p1, p2: Longint);
  begin
    TriangulatorProc(Vector3Integer(
      P0 + AddToIndices,
      P1 + AddToIndices,
      P2 + AddToIndices));
  end;

  function Verts(I: Longint): TVector3;
  begin
    if FaceIndices <> nil then
      Result := Vertices(FaceIndices^[I]) else
      Result := Vertices(I);
  end;

  { Calculate the most distant vertex from Center. }
  function GetMostDistantVertex(const Center: TVector3): Integer;
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

  { Decrease Index, until a value with Outs[Result]=false is found. }
  function PreviousNotOut(Index: Integer): Integer;
  begin
    Result := Index;
    repeat Result := Previous(Result) until not Outs[Result];
  end;

  { Look around for previous and next neighbors to the Middle vertex.
    Searches to make sure we have a good non-colinear triangle around Middle.
    Returns false (and does log message) if not possible. }
  function EarAround(const Middle: Integer; out Previous, Next: Integer;
    out EarDir: TVector3): boolean; overload;
  begin
    { Previous := previous from Middle, with different value. }
    Previous := Middle;
    repeat
      Previous := PreviousNotOut(Previous);
    until (Previous = Middle) or not TVector3.Equals(Verts(Previous), Verts(Middle));
    if Previous = Middle then
    begin
      WritelnLog('Triangulator', 'All vertexes of given polygon are equal. So polygon doesn''t contain any non-empty triangles.');
      Exit(false);
    end;

    { Next := next from Middle, resulting in valid triangle Previous-Middle-Next. }
    Next := Middle;
    repeat
      Next := NextNotOut(Next);
      EarDir := TriangleDirection(Verts(Previous), Verts(Middle), Verts(Next));
      { note: no need to check for TVector3.Equals(Verts(Next), Verts(Middle)) anywhere,
        because if EarDir is non-zero then they had to be different. }
    until (Next = Previous) or not EarDir.IsZero;
    if Next = Previous then
    begin
      WritelnLog('Triangulator', 'All vertexes of given polygon are collinear. So polygon doesn''t contain any non-empty triangles.');
      Exit(false);
    end;

    Result := true;
  end;

  function EarAround(const Middle: Integer; out Previous, Next: Integer): boolean; overload;
  var
    EarDirIgnored: TVector3;
  begin
    Result := EarAround(Middle, Previous, Next, EarDirIgnored);
  end;

var
  EpsilonForEmptyCheck: Single;

  { Should vertex Border (lying on the border of triangle V0,V1,V2)
    be treated as lying inside this triangle (and causing it non-empty).
    This is the tricky situation: border vertex

    1. *Must* cause EarFound:=false (be considered inside V0,V1,V2) in some cases.

       Consider you have a polygon with 4 vertexes,
       and 1st and 3rd are equal (have equal position).
       So a correct triangulation is 2 degenerated (colinear)
       triangles. But accidentally ignoring the border vertex means
       that we make one non-degenerated incorrect triangle:
       2-3-4 (because 1st vertex will not collide with it,
       as it's equal to 3 so it lies on the border).
       And then we would fail even more, because we're left
       with a triangle (Corners = 3) which has orientation
       reverted from the polygon. So we would make *two* incorrect
       triangles, and spill the "Impossible to find an ear" warning.

    2. *Cannot* cause EarFound:=false (be considered outside V0,V1,V2) in other cases.

       Consider two triangles joined by a single vertex
       (that is, two vertexes with equal positions).
       Clearly, a sensible triangulation exists.
       But if the middle duplicated vertex will be taken into
       account, it will block every possible ear triangle.
       Such shape would not have any possile triangulation.

    This function distinguishes between these two cases.
    Inside1/2/3 must be the edge comparison results. }
  function BorderVertexInsideTriangle(const V0, V1, V2, TriangleNormal: TVector3;
    const Inside1, Inside2, Inside3: Single;
    const Border: Integer): boolean;

    { Check collision in 2D between a ray and line segment (0,0)-(1,0).
      Ray0 and RayDirection are projected onto 2D space by assuming that
      XDirection, YDirection 3D vectors correspond to unit OX and OY vectors in 2D,
      and Origin corresponds to (0,0) in 2D.

      If ray is parallel to this line segment, answers false (because
      we use this always in case when we now that ray never lies
      on the line segment, because our line segment is chosen to be a triangle
      edge that *does not* contain Border vertex). }
    function RaySegment01Collision2D(Ray0: TVector3;
      const RayDirection: TVector3;
      const Origin, XDirection: TVector3;
      YDirection: TVector3): boolean;
    var
      R0, RDirection: TVector2;
      X, T, XDirectionLenSqr, YDirectionLenSqr: Single;
    begin
      Ray0 := Ray0 - Origin;

      { fix YDirection to be orthogonal to XDirection, to make sure projecting
        by TVector3.DotProduct is correct }
      MakeVectorsOrthoOnTheirPlane(YDirection, XDirection);

      XDirectionLenSqr := XDirection.LengthSqr;
      YDirectionLenSqr := YDirection.LengthSqr;

      R0.X := TVector3.DotProduct(Ray0, XDirection) / XDirectionLenSqr;
      R0.Y := TVector3.DotProduct(Ray0, YDirection) / YDirectionLenSqr;
      RDirection.X := TVector3.DotProduct(RayDirection, XDirection) / XDirectionLenSqr;
      RDirection.Y := TVector3.DotProduct(RayDirection, YDirection) / YDirectionLenSqr;

      if IsZero(RDirection.Y) then Exit(false);

      { we're interested now in intersection on OX axis with ray.
        R0 + RDirection * T = (X, 0), so
        T = -R0.y / RDirection.y, and
        X = R0.x + RDirection.x * T }
      T := - R0.Y / RDirection.Y;
      if T < 0 then Exit(false);

      X := R0.X + RDirection.X * T;
      Result := (0 <= X) and (X <= 1);
    end;

  var
    BorderPrevious, BorderNext: Integer;
    VBorder, BorderEarNormal, PullDirection: TVector3;
  begin
    { The two cases can be distinguished by looking at edges
      around Border. If they go out of V0-V1-V2 triangle,
      then Border is really outside (case 2. above). Otherwise, it's inside
      (case 1.). Two neighbors around Border "pull"
      it either outside or inside to our triangle, this is what
      PullDirection captures. }

    { if EarAround fails, then everything is colinear, we may as well
      let EarFound be true. This should not happen, except because
      of fp inaccuracy: if everything is colinear,
      then our EarNormal.IsZero check earlier should usually
      detect this. }
    if not EarAround(Border, BorderPrevious, BorderNext, BorderEarNormal) then
      Exit(true);

    VBorder := Verts(Border);

    PullDirection :=
      (((VBorder + Verts(BorderPrevious) + Verts(BorderNext)) / 3.0) - VBorder)
      {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif};

    { Some (at least one) of the three Inside1/2/3 values are
      now between -Epsilon .. +Epsilon, these are the edges where
      Border lies on. Other Inside1/2/3 values are < -Epsilon,
      these are the edges where Border in fully inside.
      Only the former edges should be reconsidered now,
      looking at PullDirection. If PullDirection pulls the Border vertex
      outside from the triangle, then we're Ok --- Border vertex is not
      really inside our triangle. }

    Result :=
      ( (Inside1 <= -EpsilonForEmptyCheck) and RaySegment01Collision2D(VBorder, PullDirection, V0, V1-V0, V2-V0) ) or
      ( (Inside2 <= -EpsilonForEmptyCheck) and RaySegment01Collision2D(VBorder, PullDirection, V1, V2-V1, V0-V1) ) or
      ( (Inside3 <= -EpsilonForEmptyCheck) and RaySegment01Collision2D(VBorder, PullDirection, V2, V0-V2, V1-V2) );

    { Alternative hacky way to calculate this (will work Ok if triangles
      are not too small, because of 0.01 constant) is below.
      Testcase when the two ways (above with RaySegment01Collision2D
      and below with IsPointOnTrianglePlaneWithinTriangle) give different
      answers is "manor" test from Jan Adamec's Room Arranger.

    Assert(Result = IsPointOnTrianglePlaneWithinTriangle(
      VBorder + PullDirection * 0.01,
      Triangle3(V0, V1, V2), TriangleNormal)); }

    if LogTriangulation then
      WritelnLog('Triangulation', Format('Border vertex %d (part of %d - %d - %d) considered inside triangle? %s.',
        [Border, BorderPrevious, Border, BorderNext, BoolToStr(Result, true)]));
  end;

var
  Center, EarNormal, E1, E2, E3, V0, V1, V2, PolygonNormal: TVector3;
  Corners, Start, I, P0, P1, P2: Integer;
  DistanceSqr: Single;
  EarFound, FailureWarningDone, ValidEar: boolean;
  Inside1, Inside2, Inside3: Single;
begin
  if Count = 3 then
    { For Count = 3 this is trivial, do it fast. }
    NewTriangle(0, 1, 2) else
  if Count > 3 then
  begin
    EpsilonForEmptyCheck := SingleEpsilon;
    FailureWarningDone := false;

    { calculate Center := average of all vertexes }
    Center := TVector3.Zero;
    for I := 0 to Count - 1 do
      Center := Center + Verts(I);
    Center := Center / Count;

    Outs := TBooleanList.Create;
    try
      Outs.Count := Count; { TFPGList initialized everything to false }

      { Special version for polygons with 4 points.
        After determining the 1st ear triangle, we have only one choice for
        the 2nd triangle, so use it. This is useful in case of quads at the sides
        of Extrusion, that may be "bend" in various weird ways and not really
        suitable for triangulation in 2D. See 2nd page of
        https://sourceforge.net/p/castle-engine/tickets/13/ for example. }
      if Count = 4 then
      begin
        P1 := GetMostDistantVertex(Center);
        P0 := PreviousNotOut(P1);
        P2 := NextNotOut(P1);
        NewTriangle(P0, P1, P2);
        NewTriangle(P2, NextNotOut(P2), P0);
        Exit;
      end;

      { P1 is the most distant vertex, P0 is previous, P2 is next.
        We calculate them only for the sake of calculating PolygonNormal
        (they do not determine triangulation in any other way). }
      P1 := GetMostDistantVertex(Center);
      if not EarAround(P1, P0, P2, PolygonNormal) then Exit;
      Assert(not PolygonNormal.IsZero);
      PolygonNormal := PolygonNormal.Normalize;

      if LogTriangulation then
        WritelnLog('Triangulation', Format('Most distant vertex: %d. Triangle for PolygonNormal: %d - %d - %d. Polygon normal: %s',
          [P1, P0, P1, P2, PolygonNormal.ToString]));

      Corners := Count; { Corners = always "how many Outs are false" }

      { This initial P0 value is a "border", used to prevent an infinite loop
        when we cannot find good ear triangle (which is always possible due
        to lack of floating-point precision).
        It will always be increased by first "P0 := NextNotOut(P0);".
        It must be a valid vertex index, otherwise condition "P0 = Start"
        could never occur and we would loop forever (testcase:
        change EpsilonForEmptyCheck to 0, and run on demo_models/x3d/concave.x3dv). }
      P0 := Count - 1;
      while Corners >= 3 do
      begin
        Start := P0;

        { find next ear triangle }
        repeat
          ValidEar := false;

          { increase P0. Set P1 and P2 to vertexes following P0.
            We will now consider triangle P0-P1-P2 to be removed,
            where P1 is the corner to be cut off. }
          P0 := NextNotOut(P0);
          P1 := NextNotOut(P0);
          P2 := NextNotOut(P1);

          V0 := Verts(P0);
          V1 := Verts(P1);
          V2 := Verts(P2);

          { If P0 returned back to Start value,
            then we considered every possible corner triangle and it cannot
            be cut off. IOW, we cannot find any ear triangle,
            so we cannot triangulate this polygon correctly.
            Every polygon (with >= 4 vertexes) should have at least
            2 valid "ears" to cut off, so this indicates that we have problems
            with floating-point accuracy, or we have self-intersecting
            polygon. }
          if P0 = Start then
          begin
            if LogTriangulation then
              WritelnLog('Triangulation', 'Impossible to find an "ear" to cut off, this concave polygon cannot be triangulated.');
            if not FailureWarningDone then
            begin
              WritelnWarning('Triangulator', 'Triangulation of concave polygon failed. ' + 'Polygon is probably self-intersecting (not allowed by VRML / X3D). You can use Castle Game Engine tool in castle_game_engine/examples/visualize_triangulation/ to easily observe the polygon vertexes and triangulation process.');
              FailureWarningDone := true;
            end;
            Break;
          end;

          EarNormal := TriangleDirection(V0, V1, V2);
          if EarNormal.IsZero then
          begin
            if LogTriangulation then
              WritelnLog('Triangulation', Format('Triangle %d - %d - %d is colinear, removing.', [P0, P1, P2]));
            { We know in this case we can safely remove P1.
              We cannot remove P0 or P2 (even if they are equal),
              because they are important for the shape of the polygon.
              Leave ValidEar = false, so it will not be actually returned. }
            Break;
          end;
          EarNormal := EarNormal.Normalize;

          ValidEar := true;

          { DistanceSqr is used to check that P0-P1-P2 has roughly the same
            orientation as whole polygon, not reverted. }
          DistanceSqr := PointsDistanceSqr(EarNormal, PolygonNormal);
          EarFound := DistanceSqr <= 1.0;
          if LogTriangulation then
            WritelnLog('Triangulation', Format('Does the ear %d - %d - %d have the same orientation as polygon? %s. (Ear normal: %s, distance to polygon normal: %f.)' ,
              [P0, P1, P2, BoolToStr(EarFound, true),
               EarNormal.ToString, Sqrt(DistanceSqr)]));

          { check is the ear triangle non-empty }
          if EarFound then
          begin
            { vectors orthogonal to triangle edges going *outside* from the triangle }
            E1 := (TVector3.CrossProduct(EarNormal, V0 - V1)) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif};
            E2 := (TVector3.CrossProduct(EarNormal, V1 - V2)) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif};
            E3 := (TVector3.CrossProduct(EarNormal, V2 - V0)) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif};

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
                 (I <> P2) then
              begin
                Inside1 := TVector3.DotProduct(E1, (Verts(I) - {$ifdef TRIANGULATION_EXTRA_STABILITY} (V0+V1)/2.0 {$else} V0 {$endif}) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif});
                Inside2 := TVector3.DotProduct(E2, (Verts(I) - {$ifdef TRIANGULATION_EXTRA_STABILITY} (V1+V2)/2.0 {$else} V1 {$endif}) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif});
                Inside3 := TVector3.DotProduct(E3, (Verts(I) - {$ifdef TRIANGULATION_EXTRA_STABILITY} (V2+V0)/2.0 {$else} V2 {$endif}) {$ifdef TRIANGULATION_EXTRA_STABILITY} .Normalize {$endif});

                if ( (Inside1 <= -EpsilonForEmptyCheck) and
                     (Inside2 <= -EpsilonForEmptyCheck) and
                     (Inside3 <= -EpsilonForEmptyCheck) ) or
                   ( (Inside1 <= EpsilonForEmptyCheck) and
                     (Inside2 <= EpsilonForEmptyCheck) and
                     (Inside3 <= EpsilonForEmptyCheck) and
                     BorderVertexInsideTriangle(V0, V1, V2, EarNormal,
                       Inside1, Inside2, Inside3, I) ) then
                begin
                  if LogTriangulation then
                    WritelnLog('Triangulation', Format('Triangle %d - %d - %d would not be empty: point %d would be inside.', [P0, P1, P2, I]));
                  EarFound := false;
                  Break;
                end;
              end;
          end;
        until EarFound;

        { ear triangle found, cut if off now }
        if ValidEar then NewTriangle(P0, P1, P2);
        Outs[P1] := true;
        Dec(Corners);
      end;
    finally Outs.Free end;
  end;
end;

type
  TVerticesGenerator = class
    Vertices: PVector3Array;
    VerticesCount: Integer;
    function Generate(Index: Integer): TVector3;
  end;

function TVerticesGenerator.Generate(Index: Integer): TVector3;
begin
  if Index < VerticesCount then
    Result := Vertices^[Index]
  else
    { invalid vertex index. VRML/X3D code will warn about it elsewhere,
      so do not make warning now --- just make sure we don't crash. }
    Result := TVector3.Zero;
end;

procedure TriangulateFace(
  FaceIndices: PLongintArray; Count: Integer;
  Vertices: PVector3Array; VerticesCount: Integer;
  TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);
var
  G: TVerticesGenerator;
begin
  G := TVerticesGenerator.Create;
  try
    G.Vertices := Vertices;
    G.VerticesCount := VerticesCount;
    TriangulateFace(FaceIndices, Count,
      {$ifdef FPC}@{$endif}G.Generate, TriangulatorProc, AddToIndices);
  finally FreeAndNil(G); end;
end;

procedure TriangulateConvexFace(Count: Integer;
  TriangulatorProc: TTriangulatorProc;
  AddToIndices: Longint);

  procedure NewTriangle(const p0, p1, p2: Longint);
  begin
    TriangulatorProc(Vector3Integer(
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

function IndexedConcavePolygonNormal(
  Indices: PLongintArray; Count: Integer;
  Vertices: PVector3Array; const VerticesCount: Integer;
  const ResultForIncorrectPoly: TVector3): TVector3;

{ Alternative implementation of IndexedConcavePolygonNormal
  is to do TriangulateFace on the polygon, sum normal vectors
  of all generated triangles, and normalize the result.
  However, this is very unoptimal, as TriangulateFace already calculates
  PolygonNormal (even EarNormal for each triangle) as it works.
  The current implementation replicates the beginning part of TriangulateFace job,
  in a simpler manner. }

  { Previous and Next in modulo Count (0.. Count - 1) range. }
  function Previous(const I: Integer): Integer;
  begin
    Result := I - 1;
    if Result = -1 then
      Result := Result + Count;
  end;

  function Next(const I: Integer): Integer;
  begin
    Result := (I + 1) mod Count;
  end;

  function Verts(const I: Longint): TVector3;
  var
    Index: LongInt;
  begin
    Index := Indices^[I];
    if Index < VerticesCount then
      Result := Vertices^[Index]
    else
      Result := TVector3.Zero;
  end;

  { Calculate the most distant vertex from Center. }
  function GetMostDistantVertex(const Center: TVector3): Integer;
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
  Center: TVector3;
  P0, P1, P2, I: Integer;
begin
  if Count < 3 then
    Exit(ResultForIncorrectPoly);

  { calculate Center := average of all vertexes }
  Center := TVector3.Zero;
  for I := 0 to Count - 1 do
    Center := Center + Verts(I);
  Center := Center / Count;

  { P1 is the most distant vertex. }
  P1 := GetMostDistantVertex(Center);

  { P0 := previous from P1, with different value. }
  P0 := P1;
  repeat
    P0 := Previous(P0);
  until (P0 = P1) or not TVector3.Equals(Verts(P0), Verts(P1));
  if P0 = P1 then
  begin
    { All vertexes of given polygon are equal. }
    Exit(ResultForIncorrectPoly);
  end;

  { P2 := next from P1, resulting in valid triangle P0-P1-P2. }
  P2 := P1;
  repeat
    P2 := Next(P2);
    Result := TriangleDirection(Verts(P0), Verts(P1), Verts(P2));
    { note: no need to check for TVector3.Equals(Verts(P2), Verts(P1)) anywhere,
      because if EarDir is non-zero then they had to be different. }
  until (P2 = P0) or
    { use IsPerfectlyZero, not IsZero, to not consider triangle invalid too soon.
      Testcase: demo-models/x3d/humanoid_pose_concave_test_normals.x3dv }
    not Result.IsPerfectlyZero;
  if P2 = P0 then
  begin
    { All vertexes of given polygon are collinear. }
    Exit(ResultForIncorrectPoly);
  end;

  Result := Result.Normalize;
end;

function IndexedPolygonNormal(
  Indices: PLongintArray; IndicesCount: Integer;
  Vertices: PVector3Array; const VerticesCount: Integer;
  const ResultForIncorrectPoly: TVector3; const Convex: boolean): TVector3;
begin
  if Convex then
    Result := IndexedConvexPolygonNormal (Indices, IndicesCount, Vertices, VerticesCount, ResultForIncorrectPoly)
  else
    Result := IndexedConcavePolygonNormal(Indices, IndicesCount, Vertices, VerticesCount, ResultForIncorrectPoly);
end;

end.
