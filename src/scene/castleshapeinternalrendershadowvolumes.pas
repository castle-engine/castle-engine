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
{ Internal functions for rendering shadow volumes of shapes. }
unit CastleShapeInternalRenderShadowVolumes;

{$I castleconf.inc}

interface

uses CastleVectors, CastleShapeInternalShadowVolumes,
  CastleTriangles, CastleTransform, CastleRenderPrimitives;

type
  TRenderShapeShadowVolumes = class helper for TShapeShadowVolumes
    { Rendering shadow volumes with silhouette optimization.

      This renders shadow quads of silhouette edge. Edges from ManifoldEdges
      list are used to find silhouette edge. Additionally edges from
      BorderEdges always produce shadow quads, i.e. we treat them
      like they would always be silhouette edges.

      The very idea of this optimization is that most edges are in
      ManifoldEdges and so only real silhouette edges produce shadow quads.
      In other words, BorderEdges list should not contain too many items.
      When BorderEdges contains all edges (ManifoldEdges is empty), then
      this method degenerates to a naive rendering without silhouette
      optimization. So you should try to make your models as much as
      possible resembling nice 2-manifolds. Ideally, if your mesh
      is a number of perfectly closed manifolds, and vertex ordering
      is consistent, then BorderEdges is empty, and this works perfect.

      Usually, most models are mostly 2-manifold (only the real border
      edges are, well, in BorderEdges), and this works great.
      See "VRML engine documentation" on
      [https://castle-engine.io/engine_doc.php],
      chapter "Shadows", for description and pictures of possible artifacts
      when trying to use this on models that are not 2-manifold.)

      LightCap and DarkCap say whether you want to cap your shadow volume.
      LightCap is the cap at the caster position, DarkCap is the cap in infinity.
      This is needed by z-fail method, you should set them both to @true.
      To be more optimal, you can request LightCap only if z-fail @italic(and
      the caster is inside camera frustum). For directional lights, DarkCap is
      ignored, since the volume is always closed by a single point in infinity.
    }
    procedure RenderSilhouetteShadowVolume(
      const Params: TRenderParams;
      const Mesh: TCastleRenderUnlitMesh;
      const LightPos: TVector4;
      const Transform: TMatrix4;
      const LightCap, DarkCap: boolean;
      const ForceOpaque, WholeSceneManifold: Boolean);
  end;

implementation

uses SysUtils,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleGLUtils, CastleUtils, CastleShapes, CastleImages, CastleRenderContext;

{ Return vertex Original extruded into infinity, as seen from light
  at position LightPos.

  This is designed to work only with LightPos[3] = 1. In the future, when
  need arises, this may be improved to work with any LightPos[3] <> 0.

  For LightPos[3] = 0, i.e. directional light,
  don't use this, and there's no need to do it,
  since then the extruded point is just LightPos (for any vertex).
  RenderXxxShadowVolume want to treat it specially anyway (to optimize
  drawing, since then quads degenerate to triangles).

  Note: this cannot be moved to a local function inside
  TCastleScene.RenderSilhouetteShadowVolume, as FPC 2.6.4 and 2.6.2 on Win32 (but not on Linux
  i386) generates then bad code, the loop to EdgesNow.Count doesn't finish OK,
  the index goes beyond EdgesNow.Count-1. }
function ExtrudeVertex(
  const Original: TVector3;
  const LightPos: TVector4): TVector4;
var
  LightPos3: TVector3 absolute LightPos;
begin
  { Below is the moment when we require that
    if LightPos.W <> 0 then LightPos.W = 1 (not any other non-zero value).
    Otherwise we would have to divide here LightPos3 by LightPos.W.
    Maybe in the future this requirement will be removed and we'll work
    for any LightPos in homogeneous coordinates, for now it's not really
    needed. }
  Result.X := Original.X -  LightPos3.X;
  Result.Y := Original.Y -  LightPos3.Y;
  Result.Z := Original.Z -  LightPos3.Z;
  Result.W := 0;
end;

procedure TRenderShapeShadowVolumes.RenderSilhouetteShadowVolume(
  const Params: TRenderParams;
  const Mesh: TCastleRenderUnlitMesh;
  const LightPos: TVector4;
  const Transform: TMatrix4;
  const LightCap, DarkCap: boolean;
  const ForceOpaque, WholeSceneManifold: boolean);

{ Is it worth preparing ManifoldEdges list: yes.

  At the beginning we used here the simple algorithm from
  [http://www.gamedev.net/reference/articles/article1873.asp].
  For each triangle with dot > 0, add it to the Edges list
  --- unless it's already there, in which case remove it.
  This way, at the end Edges contain all edges that have on one
  side triangle with dot > 0 and on the other side triangle with dot <= 0.
  In other words, all sihouette edges.
  (This is all assuming that model is 2-manifold,
  so each edge has exactly 2 neighbor triangles).

  But this algorithm proved to be unacceptably slow for many cases.
  While it generated much less shadow quads than naive
  RenderAllShadowVolume, the time spent in detecting the silhouette edges
  made the total time even worse than RenderAllShadowVolume.
  Obviously, that's because we started from the list of triangles,
  without any explicit information about the edges.
  The time of this algorithm was n*m, if n is the number of triangles
  and m the number of edges, and on 2-manifold n*3/2 = m so
  the time is n^2. Terrible, if you take complicated shadow caster.

  To make this faster, we have to know the connections inside the model:
  that's what ManifoldEdges list is all about. It allows us to
  implement this in time proportional to the number of edges.
}

var
  Triangles: TTrianglesShadowCastersList;
  Vertexes: TVector4List;

  { Render shadow quad from edge given by 2 points. }
  procedure RenderShadowQuad_Edge(const EdgeV0, EdgeV1: TVector3);
  var
    V0_3D, V1_3D: TVector3;
    V0, V1, V2, V3: TVector4;
  begin
    V0_3D := Transform.MultPoint(EdgeV0);
    V1_3D := Transform.MultPoint(EdgeV1);
    V0 := Vector4(V0_3D, 1);
    V1 := Vector4(V1_3D, 1);

    Vertexes.Add(V0);
    Vertexes.Add(V1);

    if LightPos[3] <> 0 then
    begin
      V2 := ExtrudeVertex(V1_3D, LightPos);
      V3 := ExtrudeVertex(V0_3D, LightPos);

      { Render quad V0..V3 (as 2 triangles, as modern OpenGL doesn't have quads). }
      Vertexes.Add(V2);

      Vertexes.Add(V0);
      Vertexes.Add(V2);
      Vertexes.Add(V3);
    end else
      { finish triangle }
      Vertexes.Add(LightPos);
  end;

  { Render shadow quad from PEdge. }
  procedure RenderShadowQuad_EdgePtr(const EdgePtr: PEdge;
    const P0Index, P1Index: Cardinal);
  var
    TrianglePtr: PTriangle3;
  begin
    TrianglePtr := PTriangle3(Triangles.Ptr(EdgePtr^.Triangles[0]));
    RenderShadowQuad_Edge(
      TrianglePtr^.Data[(EdgePtr^.VertexIndex + P0Index) mod 3],
      TrianglePtr^.Data[(EdgePtr^.VertexIndex + P1Index) mod 3]
    );
  end;

  { Render remaining Vertexes, if any.
    The Vertexes list is always empty after this. }
  procedure FlushVertexes;
  begin
    if Vertexes.Count <> 0 then
    begin
      Mesh.SetVertexes(Vertexes, true);
      Mesh.Render(pmTriangles);
      Vertexes.Clear;
    end;
  end;

  { We initialize TrianglesPlaneSide and render caps in one step,
    this way we have to iterate over Triangles only once, and in case
    of PlaneSide_NotIdentity and rendering caps --- we have to transform
    each triangle only once. }
  procedure InitializeTrianglesPlaneSideAndRenderCaps(
    TrianglesPlaneSide: TBooleanList;
    LightCap, DarkCap: boolean);

    procedure RenderCaps(const T: TTriangle3);
    begin
      if LightCap then
      begin
        Vertexes.Add(Vector4(T.Data[0], 1));
        Vertexes.Add(Vector4(T.Data[1], 1));
        Vertexes.Add(Vector4(T.Data[2], 1));
      end;

      if DarkCap then
      begin
        Vertexes.Add(ExtrudeVertex(T.Data[2], LightPos));
        Vertexes.Add(ExtrudeVertex(T.Data[1], LightPos));
        Vertexes.Add(ExtrudeVertex(T.Data[0], LightPos));
      end;
    end;

    function PlaneSide(const T: TTriangle3): boolean;
    var
      Plane: TVector4;
      TriangleTransformed: TTriangle3;
    begin
      TriangleTransformed.Data[0] := Transform.MultPoint(T.Data[0]);
      TriangleTransformed.Data[1] := Transform.MultPoint(T.Data[1]);
      TriangleTransformed.Data[2] := Transform.MultPoint(T.Data[2]);
      Plane := TriangleTransformed.Plane;
      Result := (Plane.X * LightPos.X +
                 Plane.Y * LightPos.Y +
                 Plane.Z * LightPos.Z +
                 Plane.W * LightPos.W) > 0;
      if Result then RenderCaps(TriangleTransformed);
    end;

    { Comments for Opaque/TransparentTrianglesBegin/End:

      It's crucial to set glDepthFunc(GL_NEVER) for LightCap.
      This way we get proper self-shadowing. Otherwise, LightCap would
      collide in z buffer with the object itself.

      Setting glDepthFunc(GL_NEVER) for DarkCap also is harmless and OK.
      Proof: if there's anything on this pixel, then indeed the depth test
      would fail. If the pixel is empty (nothing was rasterized there),
      then the depth test wouldn't fail... but also, in this case value in
      stencil buffer will not matter, it doesn't matter if this pixel
      is in shadow or not because there's simply nothing there.

      And it allows us to render both LightCap and DarkCap in one
      GL_TRIANGLES pass, in one iteration over Triangles list, which is
      good for speed.

      Some papers propose other solution:
        RenderContext.PolygonOffsetEnable(1, 1);
      but this is no good for use, because it cannot be applied
      to DarkCap (otherwise DarkCap in infinity (as done by ExtrudeVertex)
      would go outside of depth range (even for infinite projection,
      as glPolygonOffset works already after the vertex is transformed
      by projection), and this would make DarkCap not rendered
      (outside of depth range)).

      If you consider that some shadow casters and receivers may
      be partially transparent (that is, rendered without writing
      to depth buffer) then the above reasoning is not so simple:

      - There's no way to handle transparent
        objects (that are not recorded in depth buffer) as shadow receivers.
        Rendering them twice with blending would result in wrong blending
        modes applied anyway. So TGLShadowVolumeRenderer.Render renders them
        at the end, as last pass.

        This means that "glDepthFunc(GL_NEVER) for DarkCap" is still
        Ok: if on some pixel there was only transparent object visible,
        then stencil value of this pixel is wrong, but transparent object
        will never be rendered in shadowed state --- so it will not
        look at stencil value.

        For LightCap, situation is worse. Even if the transparent
        object is only shadow caster (not receiver), still problems
        may arise due to glDepthFunc(GL_NEVER): imagine you have
        a transparent object casting shadow on non-transparent object
        (see e.g. demo_models/shadow_volumes/ghost_shadow.wrl).
        This means that you can look through the shadow casting
        (transp) object and see shadow receiving (opaque) object,
        that may or may not be in shadow on speciic pixel.
        Which means that glDepthFunc(GL_NEVER) is wrong for LightCap:
        the transparent object doesn't hide the shadow on the screen,
        and the depth test shouldn't fail. Which means that for transparent
        objects, we cannot do glDepthFunc(GL_NEVER).

      - What to do?

        The trick
          RenderContext.PolygonOffsetEnable(1, 1);
        makes light cap rendering working for both transparent and opaque
        objects, but it's not applicable to dark cap. Moreover,
        using glPolygonOffset always feels dirty.

        Solution: we decide to handle transparent objects separately.
        We note that for transparent shadow casters
        actually no tweaks to caps rendering should be done.
        No glPolygonOffset, no glDepthFunc(GL_NEVER) needed: light cap
        should be tested as usual. (Since transparent object is not written
        to depth buffer, it will not collide in depth buffer with it's
        light cap).

        This means that is we'll just split triangles list into
        transparent and opaque ones, then the only complication needed
        is to switch glDepthFunc(GL_NEVER) trick *off* for transparent
        triangles. And all works fast.

      - There's actually one more note: for transparent objects,
        caps are always needed (even with zpass).
        Note that this means that whole 2-manifold part must have
        caps.

        This also means that joining one 2-manifold path from some transparent
        and some opaque triangles will not work. (as then some parts
        may have caps (like transparent ones) and some note
        (like opaque ones with zpass)).

        TODO: implement above.
    }

  var
    SavedDepthFunc: TDepthFunction;

    procedure OpaqueTrianglesBegin;
    begin
      if LightCap or DarkCap then
      begin
        FlushVertexes;

        SavedDepthFunc := RenderContext.DepthFunc;
        RenderContext.DepthFunc := dfNever;
      end;
    end;

    procedure OpaqueTrianglesEnd;
    begin
      if LightCap or DarkCap then
      begin
        FlushVertexes;

        RenderContext.DepthFunc := SavedDepthFunc;
      end;
    end;

    procedure TransparentTrianglesBegin;
    begin
      { Caps are always needed, doesn't depend on zpass/zfail.
        Well, for dark cap we can avoid them if the light is directional. }
      LightCap := true;
      DarkCap := LightPos.W <> 0;
    end;

    procedure TransparentTrianglesEnd;
    begin
    end;

  var
    TrianglePtr: PTriangle3;
    I: Integer;
  begin
    TrianglesPlaneSide.Count := Triangles.Count;
    TrianglePtr := PTriangle3(Triangles.L);

    { If light is directional, no need to render dark cap }
    DarkCap := DarkCap and (LightPos.W <> 0);

    if ForceOpaque or not (TShape(FShape).AlphaChannel = acBlending) then
      OpaqueTrianglesBegin
    else
      TransparentTrianglesBegin;

    for I := 0 to Triangles.Count - 1 do
    begin
      TrianglesPlaneSide.L[I] := PlaneSide(TrianglePtr^);
      Inc(TrianglePtr);
    end;

    if ForceOpaque or not (TShape(FShape).AlphaChannel = acBlending) then
      OpaqueTrianglesEnd
    else
      TransparentTrianglesEnd;
  end;

var
  I: Integer;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TBooleanList;
  EdgesNow: TEdgeList;
  EdgePtr: PEdge;
begin
  Assert(ManifoldEdges <> nil);

  { If the model is not perfect 2-manifold, do not render it's shadow volumes.
    This is good default behavior, as shadow volumes on non-manifolds
    make weird artifacts.

    Developer can override it with WholeSceneManifold though. }
  if (BorderEdges.Count <> 0) and
     (not WholeSceneManifold) then
    Exit;

  Mesh.ModelViewProjection := RenderContext.ProjectionMatrix * Params.RenderingCamera.CurrentMatrix;

  Triangles := TrianglesListShadowCasters;

  { Nil local variables, to later free by one finally clause. }
  TrianglesPlaneSide := nil;
  Vertexes := nil;

  try
    TrianglesPlaneSide := TBooleanList.Create;
    Vertexes := TVector4List.Create;

    InitializeTrianglesPlaneSideAndRenderCaps(TrianglesPlaneSide,
      LightCap, DarkCap);

    { for each 2-manifold edge, possibly render it's shadow quad }
    EdgesNow := ManifoldEdges;
    EdgePtr := PEdge(EdgesNow.L);
    for I := 0 to EdgesNow.Count - 1 do
    begin
      PlaneSide0 := TrianglesPlaneSide.L[EdgePtr^.Triangles[0]];
      PlaneSide1 := TrianglesPlaneSide.L[EdgePtr^.Triangles[1]];

      { Only if PlaneSide0 <> PlaneSide1 it's a silhouette edge,
        so only then render it's shadow quad.

        We want to have consistent CCW orientation of shadow quads faces,
        so that face is oriented CCW <=> you're looking at it from outside
        (i.e. it's considered front face of this shadow quad).
        This is needed, since user of this method may want to do culling
        to eliminate back or front faces.

        TriangleDirection(T) indicates direction that goes from CCW triangle side
        (that's guaranteed by the way TriangleDir calculates plane dir).
        So PlaneSideX is @true if LightPos is on CCW side of appropriate
        triangle. So if PlaneSide0 the shadow quad is extended
        in reversed Triangles[0] order, i.e. like 1, 0, Extruded0, Extruded1.
        Otherwise, in normal Triangles[0], i.e. 0, 1, Extruded1, Extruded0.

        Just draw it, the triangle corners numbered with 0,1,2 in CCW and
        imagine that you want the shadow quad to be also CCW on the outside,
        it will make sense then :) }
      if PlaneSide0 and not PlaneSide1 then
        RenderShadowQuad_EdgePtr(EdgePtr, 1, 0)
      else
      if PlaneSide1 and not PlaneSide0 then
        RenderShadowQuad_EdgePtr(EdgePtr, 0, 1);

      Inc(EdgePtr);
    end;

    { For each border edge, always render it's shadow quad.
      This is used now only if WholeSceneManifold
      (otherwise at the beginning of this method,
      we exit if BorderEdges.Count <> 0). }
    EdgesNow := BorderEdges;
    EdgePtr := PEdge(EdgesNow.L);
    for I := 0 to EdgesNow.Count - 1 do
    begin
      PlaneSide0 := TrianglesPlaneSide.L[EdgePtr^.Triangles[0]];

      { Note that even for BorderEdges that are actually manifold but from
        different shapes (so have Triangles[1] = High(Cardinal)),
        we don't check PlaneSide1, so we don't render them under the same
        conditions as ManifoldEdges.

        While this would be more optimal (less shadow quads)
        but force such BorderEdges to have consistent order (reverted)
        in both shapes. Right now our CalculateDetectedWholeSceneManifold
        allows any order.

        Moreover, it means that forcing 2-manifold by
        RenderOptions.WholeSceneManifold is a valid optimization at loading,
        without any adverse effects. RenderOptions.WholeSceneManifold doesn't
        set "Triangles[1] = High(Cardinal)" on any edge (which is OK since we
        don't use it for rendering), and it avoids doing
        CalculateDetectedWholeSceneManifold.
      }

      { We want to have consistent CCW orientation of shadow quads faces,
        so that face is oriented CCW <=> you're looking at it from outside
        (i.e. it's considered front face of this shadow quad).
        This is needed, since user of this method may want to do culling
        to eliminate back or front faces.

        TriangleDirection(T) indicates direction that goes from CCW triangle side
        (that's guaranteed by the way TriangleDir calculates plane dir).
        So PlaneSide0 is true if LightPos is on CCW side of appropriate
        triangle. So if PlaneSide0, the shadow quad is extended
        in the direction of TriangleIndex, like 1, 0, Extruded0, Extruded1. }
      if PlaneSide0 then
        RenderShadowQuad_EdgePtr(EdgePtr, 1, 0)
      {
      // Do not render shadow quad from other border edge, this would break rendering
      // (testcase: examples/viewport_and_scenes/shadow_volumes_whole_scene_manifold/ ).
      // The other shape, that shares this manifold edge, will render the corresponding shadow quad for it.
      else
        RenderShadowQuad_EdgePtr(EdgePtr, 0, 1)};

      Inc(EdgePtr);
    end;

    FlushVertexes;
  finally
    FreeAndNil(TrianglesPlaneSide);
    FreeAndNil(Vertexes);
  end;
end;

end.
