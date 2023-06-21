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

{ Rendering with occlusion culling.
  @exclude }
unit CastleInternalOcclusionCulling;

{$I castleconf.inc}

interface

uses
  CastleVectors, CastleSceneCore, CastleSceneInternalShape,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleGLUtils, CastleRenderContext, CastleFrustum, CastleGLShaders,
  CastleBoxes, CastleTransform, CastleRenderPrimitives;

type
  TShapeRenderEvent = procedure (const Shape: TCollectedShape;
    const Params: TRenderParams) of object;

  TOcclusionCullingUtilsRenderer = class
  strict private
    RenderBox: TCastleRenderUnlitMesh;
    OcclusionBoxState: boolean;
    SavedDepthBufferUpdate: Boolean;
    SavedColorChannels: TColorChannels;
    SavedCullFace: Boolean;
    procedure GLContextOpen;
    procedure OcclusionBoxStateBegin;
  private
    procedure DrawBox(const Box: TBox3D);
  public
    ProjectionMatrix: TMatrix4;
    CameraMatrix: TMatrix4;
    SceneMatrix: TMatrix4;
    procedure GLContextClose;
    procedure OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
  end;

  TOcclusionCullingRenderer = class
  strict private
    FUtils: TOcclusionCullingUtilsRenderer;
  public
    constructor Create;
    destructor Destroy; override;
    property Utils: TOcclusionCullingUtilsRenderer read FUtils;
    procedure Render(const CollectedShape: TCollectedShape;
      const Params: TRenderParams;
      const RenderShape: TShapeRenderEvent);
  end;

implementation

uses SysUtils,
  CastleClassUtils, CastleInternalShapeOctree, CastleRenderOptions,
  CastleUtils;

{ TOcclusionCullingUtilsRenderer ------------------------------------------------- }

procedure TOcclusionCullingUtilsRenderer.GLContextOpen;
const
  Indexes: array [0..35] of UInt16 =
  (
    // triangles:         // quads:
    0, 1, 3, 0, 3, 2,     // 0, 1, 3, 2,
    1, 5, 7, 1, 7, 3,     // 1, 5, 7, 3,
    5, 4, 6, 5, 6, 7,     // 5, 4, 6, 7,
    4, 0, 2, 4, 2, 6,     // 4, 0, 2, 6,
    2, 3, 7, 2, 7, 6,     // 2, 3, 7, 6,
    0, 4, 5, 0, 5, 1      // 0, 4, 5, 1
  );
begin
  RenderBox := TCastleRenderUnlitMesh.Create(false);
  RenderBox.SetIndexes(Indexes);
end;

procedure TOcclusionCullingUtilsRenderer.GLContextClose;
begin
  FreeAndNil(RenderBox);
end;

procedure TOcclusionCullingUtilsRenderer.OcclusionBoxStateBegin;
begin
  if not OcclusionBoxState then
  begin
    if RenderBox = nil then
      GLContextOpen;

    SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
    RenderContext.DepthBufferUpdate := false;

    SavedColorChannels := RenderContext.ColorChannels;
    RenderContext.ColorChannels := [];

    SavedCullFace := RenderContext.CullFace;
    RenderContext.CullFace := false;

    { Do not alpha test.
      This also means that texture enabled for fixed-function will be meaningless,
      it will not affect occlusion result, which is good.

      Note that we don't care to save/restore this state,
      as each state will reenable alpha test by RenderContext.FixedFunctionAlphaTestEnable
      if necessary. }
    RenderContext.FixedFunctionAlphaTestDisable;

    OcclusionBoxState := true;
  end;
end;

procedure TOcclusionCullingUtilsRenderer.OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
begin
  if OcclusionBoxState then
  begin
    if RestoreDefaults then
    begin
      RenderContext.DepthBufferUpdate := true;
      RenderContext.ColorChannels := [0..3];
      RenderContext.CullFace := false;
    end else
    begin
      RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
      RenderContext.ColorChannels := SavedColorChannels;
      RenderContext.CullFace := SavedCullFace;
    end;

    OcclusionBoxState := false;
  end;
end;

procedure TOcclusionCullingUtilsRenderer.DrawBox(const Box: TBox3D);
var
  Verts: array [0..7] of TVector4;
begin
  if Box.IsEmpty then Exit;

  OcclusionBoxStateBegin;

  { Verts index in octal notation indicates which of 8 vertexes it is. }
  Verts[0] := Vector4(Box.Data[0], 1);
  Verts[1] := Vector4(Box.Data[0], 1); Verts[1].Data[0] := Box.Data[1][0];
  Verts[2] := Vector4(Box.Data[0], 1); Verts[2].Data[1] := Box.Data[1][1];
  Verts[4] := Vector4(Box.Data[0], 1); Verts[4].Data[2] := Box.Data[1][2];

  Verts[3] := Vector4(Box.Data[1], 1); Verts[3].Data[2] := Box.Data[0][2];
  Verts[5] := Vector4(Box.Data[1], 1); Verts[5].Data[1] := Box.Data[0][1];
  Verts[6] := Vector4(Box.Data[1], 1); Verts[6].Data[0] := Box.Data[0][0];
  Verts[7] := Vector4(Box.Data[1], 1);

  RenderBox.ModelViewProjection := ProjectionMatrix * CameraMatrix * SceneMatrix;
  RenderBox.SetVertexes(Verts, true);
  RenderBox.Render(pmTriangles);
end;

{ TOcclusionQuery ------------------------------------------------------------ }

type
  TOcclusionQuery = class
  public
    constructor Create;
    destructor Destroy; override;

  public
    Id: TGLuint;

    Node: TShapeOctreeNode;

    function Available: LongBool;
    function GetResult: TGLuint;
  end;

constructor TOcclusionQuery.Create;
begin
  inherited;
  glGenQueries(1, @Id);
end;

destructor TOcclusionQuery.Destroy;
begin
  glDeleteQueries(1, @Id);
  inherited;
end;

function TOcclusionQuery.Available: LongBool;
begin
  Assert(SizeOf(LongBool) = SizeOf(TGLuint));
  glGetQueryObjectuiv(Id, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

function TOcclusionQuery.GetResult: TGLuint;
begin
  glGetQueryObjectuiv(Id, GL_QUERY_RESULT, @Result);
end;

{ TOcclusionCullingRenderer ---------------------------------------------- }

const
  QueryTarget =
    {$ifndef OpenGLES} GL_SAMPLES_PASSED
    { Following https://registry.khronos.org/OpenGL-Refpages/es3/html/glBeginQuery.xhtml
      it seems better to use GL_ANY_SAMPLES_PASSED_CONSERVATIVE: it may sometimes return
      that object is visible when it is not, but it is faster. }
    {$else} GL_ANY_SAMPLES_PASSED_CONSERVATIVE
    {$endif};

constructor TOcclusionCullingRenderer.Create;
begin
  inherited Create;
  FUtils := TOcclusionCullingUtilsRenderer.Create;
end;

destructor TOcclusionCullingRenderer.Destroy;
begin
  FreeAndNil(FUtils);
  inherited;
end;

procedure TOcclusionCullingRenderer.Render(const CollectedShape: TCollectedShape;
  const Params: TRenderParams; const RenderShape: TShapeRenderEvent);
var
  Shape: TGLShape;
  SampleCount: TGLuint;
begin
  Shape := CollectedShape.Shape;

  if Shape.OcclusionQueryId = 0 then
  begin
    glGenQueries(1, @Shape.OcclusionQueryId);
    Shape.OcclusionQueryAsked := false;
  end;

  if Shape.OcclusionQueryAsked then
    glGetQueryObjectuiv(Shape.OcclusionQueryId, GL_QUERY_RESULT,
      @SampleCount)
  else
    SampleCount := 1; { if not asked, assume it's visible }

  { Do not do occlusion query (although still use results from previous
    query) if we're within stencil test (like in InShadow = false pass
    of shadow volumes). This would incorrectly mark some shapes
    as non-visible (just because they don't pass stencil test on any pixel),
    while in fact they should be visible in the very next
    (with InShadow = true) render pass. }

  if Params.StencilTest = 0 then
    glBeginQuery(QueryTarget, Shape.OcclusionQueryId);

    if SampleCount > 0 then
    begin
      RenderShape(CollectedShape, Params);
    end else
    begin
      { Object was not visible in the last frame.
        In this frame, only render it's bounding box, to test
        occlusion query. This is the speedup of using occlusion query:
        we render only bbox. }

      Utils.SceneMatrix := CollectedShape.SceneTransform;
      Utils.DrawBox(Shape.BoundingBox);
      if (Params.InternalPass = 0) {TODO: and not Scene.ExcludeFromStatistics} then
        Inc(Params.Statistics.BoxesOcclusionQueriedCount);
    end;

  if Params.StencilTest = 0 then
  begin
    glEndQuery(QueryTarget);
    Shape.OcclusionQueryAsked := true;
  end;
end;

end.
