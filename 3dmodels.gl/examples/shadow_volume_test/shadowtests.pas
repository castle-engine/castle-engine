{
  Copyright 2003-2007 Michalis Kamburelis.

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

{ Utilities for shadow_volume_test program. }
unit ShadowTests;

interface

uses VectorMath, VRMLFlatSceneGL, VRMLFlatScene, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TQuad4Single = array[0..3] of TVector4Single;
  PQuad4Single = ^TQuad4Single;

  TDynArrayItem_1 = TQuad4Single;
  PDynArrayItem_1 = PQuad4Single;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Quad4Single = TInfiniteArray_1;
  PArray_Quad4Single = PInfiniteArray_1;
  TDynQuad4SingleArray = TDynArray_1;

{ These things were part of VRMLFlatSceneGL unit once, but I removed them
  from there, since I discovered that they are not useful methods of
  implementing shadow volumes.

  RenderFrontShadowQuads and RenderBackShadowQuads
  removed from VRMLFlatSceneGL because:
  1. We can check front/back using OpenGL as easy
  2. Actually, checking using OpenGL is better because it's more accurate
  3. (not to mention that this renders all edges, not just silhouette edges) }

{ RenderFrontShadowQuads
  renders only quads front facing CameraPos,
  RenderBackShadowQuads renders the rest of the quads.

  Uses TrianglesList(false) (so you may prefer to prepare it
  before, e.g. by calling PrepareRender with
  prTrianglesListNonOverTriangulate).

  All the commands passed to OpenGL by this methods are:
  glBegin, sequence of glVertex, then glEnd.

  When rendering front shadow quads, we actually calculate
  also back shadow quads. To RenderFrontShadowQuads you
  just pass instance of TDynQuad4SingleArray (don't care
  about it's contents, all will be initialized by RenderFrontShadowQuads).
  To RenderBackShadowQuads you must pass unmodified SavedShadowQuads
  as received by previous RenderFrontShadowQuads call.
  You can also use versions that don't take SavedShadowQuads argument
  at all --- they will just internally use TDynQuad4Single instance
  inside this object (so they are a little less flexible,
  and in some cases may unnecessarily waste memory).

  @groupBegin }
procedure RenderFrontShadowQuads(
  Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single;
  SavedShadowQuads: TDynQuad4SingleArray); overload;

procedure RenderBackShadowQuads(
  Scene: TVRMLFlatSceneGL;
  SavedShadowQuads: TDynQuad4SingleArray); overload;

procedure RenderFrontShadowQuads(
  Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single); overload;

procedure RenderBackShadowQuads(Scene: TVRMLFlatSceneGL); overload;
{ @groupEnd }

{ Render silhouette edges.
  This is actually a modified implementation of
  TVRMLFlatSceneGL.RenderSilhouetteShadowQuads: instead of rendering
  shadow quad for each silhouette edge, the edge is simply rendered
  as OpenGL line. }
procedure RenderSilhouetteEdges(
  Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const Transform: TMatrix4Single);

{ Render all Scene.BorderEdges as lines. }
procedure RenderBorderEdges(
  Scene: TVRMLFlatSceneGL;
  const Transform: TMatrix4Single);

{$undef read_interface}

implementation

uses SysUtils, KambiGLUtils, GL, GLU, GLExt;

{$define read_implementation}
{$I dynarray_1.inc}

{ This returns vertex Original extruded into infinity, as seen from light
  at position LightPos.

  This is designed to work only with LightPos[3] = 1. In the future, when
  need arises, this may be improved to work with any LightPos[3] <> 0.

  For LightPos[3] = 0, i.e. directional light,
  don't use this, and there's no need to do it,
  since then the extruded point is just LightPos (for any vertex).
  RenderXxxShadowQuads want to treat it specially anyway (to optimize
  drawing, since then quads degenerate to triangles). }
function ExtrudeVertex(
  const Original: TVector3Single;
  const LightPos: TVector4Single): TVector4Single;
var
  LightPos3: TVector3Single absolute LightPos;
begin
  { Below is the moment when we require that
    if LightPos[3] <> 0 then LightPos[3] = 1 (not any other non-zero value).
    Otherwise we would have to divide here LightPos3 by LightPos[3].
    Maybe in the future this requirement will be removed and we'll work
    for any LightPos in homogenous coordinates, for now it's not really
    needed. }
  Result[0] := Original[0] -  LightPos3[0];
  Result[1] := Original[1] -  LightPos3[1];
  Result[2] := Original[2] -  LightPos3[2];
  Result[3] := 0;
end;

procedure RenderFrontShadowQuads(Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single;
  SavedShadowQuads: TDynQuad4SingleArray);

{ It's important here that TrianglesList guarentees that only valid
  triangles are included. Otherwise degenerate triangles could make
  shadow volumes rendering result bad. }

  { Let SQ = shadow quad constructed by extending P0 and P1 by lines
    from LightPos. POther is given here as a reference of the "inside"
    part of triangle: let P = plane formed by P0, P1 and LightPos,
    if CameraPos is on the same side of plane P as POther then
    SQ is back-facing, else SQ is front-facing.
    Let SQFront:="is SQ front facing".
    If SQFront = Front then this procedure renders SQ, else is does not. }
  procedure MaybeRenderShadowQuad(const P0, P1, POther: TVector3Single;
    const PExtruded0, PExtruded1: TVector4Single; const SQFront: boolean);
  var
    QuadPtr: PQuad4Single;
  begin
    if SQFront then
    begin
      glVertexv(P0);
      glVertexv(P1);
      glVertexv(PExtruded1);
      glVertexv(PExtruded0);
    end else
    begin
      SavedShadowQuads.IncLength;
      QuadPtr := SavedShadowQuads.Pointers[SavedShadowQuads.High];
      QuadPtr^[0] := Vector4Single(P0, 1);
      QuadPtr^[1] := Vector4Single(P1, 1);
      QuadPtr^[2] := PExtruded1;
      QuadPtr^[3] := PExtruded0;
    end;
  end;

  procedure MaybeRenderShadowQuad(const P0, P1, POther: TVector3Single;
    const PExtruded: TVector4Single; const SQFront: boolean);
  var
    QuadPtr: PQuad4Single;
  begin
    if SQFront then
    begin
      glVertexv(P0);
      glVertexv(P1);
      glVertexv(PExtruded);
    end else
    begin
      SavedShadowQuads.IncLength;
      QuadPtr := SavedShadowQuads.Pointers[SavedShadowQuads.High];
      QuadPtr^[0] := Vector4Single(P0, 1);
      QuadPtr^[1] := Vector4Single(P1, 1);
      QuadPtr^[2] := PExtruded;
      { This is poor code, we should have separate Quad array for triangles
        (for directional light case). }
      QuadPtr^[3] := PExtruded;
    end;
  end;

var
  I: Integer;
  Triangles: TDynTriangle3SingleArray;
  T0, T1, T2: TVector3Single;
  TExtruded0, TExtruded1, TExtruded2: TVector4Single;
  SQPlanes: array [0..2] of TVector4Single;
  SQFronts: array [0..2] of boolean;
  LightPos3: TVector3Single absolute LightPos;
begin
  Triangles := Scene.TrianglesList(false);

  SavedShadowQuads.Count := 0;
  SavedShadowQuads.AllowedCapacityOverflow := Triangles.Count * 3;

  if LightPos[3] <> 0 then
    glBegin(GL_QUADS) else
    glBegin(GL_TRIANGLES);

    for I := 0 to Triangles.Count - 1 do
    begin
      { calculate T := Triangles[I] transformed by TrianglesTransform }
      T0 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][0]);
      T1 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][1]);
      T2 := MultMatrixPoint(TrianglesTransform, Triangles.Items[I][2]);

      { First calculate all three SQPlanes and all three
        SQFronts. This is because we *have* to catch the situation
        when all SQFronts are equal. This may happen when the triangle
        (T0, T1, T2) and  LightPos are on the same plane. In this case,
        we should just ignore the triangle.

        TODO: this assumes light is positional. I'm too lazy to implement
        directional lights here. }

      SQPlanes[0] := TrianglePlane(T0, T1, LightPos3);
      SQPlanes[1] := TrianglePlane(T1, T2, LightPos3);
      SQPlanes[2] := TrianglePlane(T2, T0, LightPos3);

      SQFronts[0] := not PointsSamePlaneSides(T2, CameraPos, SQPlanes[0]);
      SQFronts[1] := not PointsSamePlaneSides(T0, CameraPos, SQPlanes[1]);
      SQFronts[2] := not PointsSamePlaneSides(T1, CameraPos, SQPlanes[2]);

      if LightPos[3] <> 0 then
      begin
        TExtruded0 := ExtrudeVertex(T0, LightPos);
        TExtruded1 := ExtrudeVertex(T1, LightPos);
        TExtruded2 := ExtrudeVertex(T2, LightPos);

        { not ((SQFronts[0] = SQFronts[1]) and (SQFronts[1] = SQFronts[2])) =
          not (not (SQFronts[0] xor SQFronts[1]) and not (SQFronts[1] xor SQFronts[2])) =
          (SQFronts[0] xor SQFronts[1]) or (SQFronts[1] xor SQFronts[2]) }

        if (SQFronts[0] xor SQFronts[1]) or (SQFronts[1] xor SQFronts[2]) then
        begin
          MaybeRenderShadowQuad(T0, T1, T2, TExtruded0, TExtruded1, SQFronts[0]);
          MaybeRenderShadowQuad(T1, T2, T0, TExtruded1, TExtruded2, SQFronts[1]);
          MaybeRenderShadowQuad(T2, T0, T1, TExtruded2, TExtruded0, SQFronts[2]);
        end;
      end else
      begin
        { For directional light, this is a little simpler, since
          extruded vertex is just equal to LightPos. }
        if (SQFronts[0] xor SQFronts[1]) or (SQFronts[1] xor SQFronts[2]) then
        begin
          MaybeRenderShadowQuad(T0, T1, T2, LightPos, SQFronts[0]);
          MaybeRenderShadowQuad(T1, T2, T0, LightPos, SQFronts[1]);
          MaybeRenderShadowQuad(T2, T0, T1, LightPos, SQFronts[2]);
        end;
      end;
    end;
  glEnd;
end;

procedure RenderBackShadowQuads(
  Scene: TVRMLFlatSceneGL;
  SavedShadowQuads: TDynQuad4SingleArray);
var
  I: Integer;
begin
  { That's brutally simple, just render all the quads. }
  glBegin(GL_QUADS);
    for I := 0 to SavedShadowQuads.High do
    begin
      glVertexv(SavedShadowQuads.Items[I][0]);
      glVertexv(SavedShadowQuads.Items[I][1]);
      glVertexv(SavedShadowQuads.Items[I][2]);
      glVertexv(SavedShadowQuads.Items[I][3]);
    end;
  glEnd;
end;

var
  DefaultSavedShadowQuads: TDynQuad4SingleArray;

procedure RenderFrontShadowQuads(
  Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const CameraPos: TVector3Single;
  const TrianglesTransform: TMatrix4Single);
begin
  RenderFrontShadowQuads(Scene, LightPos, CameraPos, TrianglesTransform,
    DefaultSavedShadowQuads);
end;

procedure RenderBackShadowQuads(Scene: TVRMLFlatSceneGL);
begin
  RenderBackShadowQuads(Scene, DefaultSavedShadowQuads);
end;

procedure RenderSilhouetteEdges(
  Scene: TVRMLFlatSceneGL;
  const LightPos: TVector4Single;
  const Transform: TMatrix4Single);

var
  Triangles: TDynTriangle3SingleArray;
  EdgePtr: PManifoldEdge;

  procedure RenderShadowQuad(
    const P0Index, P1Index: Cardinal);
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Triangles.Pointers[EdgePtr^.Triangles[0]];
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    V0 := MultMatrixPoint(Transform, EdgeV0^);
    V1 := MultMatrixPoint(Transform, EdgeV1^);

    glVertexv(V0);
    glVertexv(V1);
  end;

  function PlaneSide(const T: TTriangle3Single): boolean;
  var
    Plane: TVector4Single;
  begin
    Plane := TrianglePlane(
      MultMatrixPoint(Transform, T[0]),
      MultMatrixPoint(Transform, T[1]),
      MultMatrixPoint(Transform, T[2]));
    Result := (Plane[0] * LightPos[0] +
               Plane[1] * LightPos[1] +
               Plane[2] * LightPos[2] +
               Plane[3] * LightPos[3]) > 0;
  end;

var
  I: Integer;
  TrianglePtr: PTriangle3Single;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TDynBooleanArray;
  Edges: TDynManifoldEdgeArray;
begin
  glColor4f(1, 1, 0, 0.3);
  glBegin(GL_LINES);
    Triangles := Scene.TrianglesList(false);
    Edges := Scene.ManifoldEdges;

    TrianglesPlaneSide := TDynBooleanArray.Create;
    try
      { calculate TrianglesPlaneSide array }
      TrianglesPlaneSide.Count := Triangles.Count;
      TrianglePtr := Triangles.Pointers[0];
      for I := 0 to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.Items[I] := PlaneSide(TrianglePtr^);
        Inc(TrianglePtr);
      end;

      { for each edge, possibly render it's shadow quad }
      EdgePtr := Edges.Pointers[0];
      for I := 0 to Edges.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.Items[EdgePtr^.Triangles[0]];
        PlaneSide1 := TrianglesPlaneSide.Items[EdgePtr^.Triangles[1]];

        if PlaneSide0 <> PlaneSide1 then
          RenderShadowQuad(0, 1);

        Inc(EdgePtr);
      end;

    finally FreeAndNil(TrianglesPlaneSide) end;
  glEnd;
end;

procedure RenderBorderEdges(
  Scene: TVRMLFlatSceneGL;
  const Transform: TMatrix4Single);
var
  Triangles: TDynTriangle3SingleArray;
  EdgePtr: PBorderEdge;

  procedure RenderEdge;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Triangles.Pointers[EdgePtr^.TriangleIndex];
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + 0) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + 1) mod 3];

    V0 := MultMatrixPoint(Transform, EdgeV0^);
    V1 := MultMatrixPoint(Transform, EdgeV1^);

    glVertexv(V0);
    glVertexv(V1);
  end;

var
  I: Integer;
  Edges: TDynBorderEdgeArray;
begin
  glColor4f(0, 0, 1, 0.3);
  glBegin(GL_LINES);
    Triangles := Scene.TrianglesList(false);
    Edges := Scene.BorderEdges;

    { for each edge, render it }
    EdgePtr := Edges.Pointers[0];
    for I := 0 to Edges.Count - 1 do
    begin
      RenderEdge;
      Inc(EdgePtr);
    end;
  glEnd;
end;

initialization
  DefaultSavedShadowQuads := TDynQuad4SingleArray.Create;
finalization
  FreeAndNil(DefaultSavedShadowQuads);
end.