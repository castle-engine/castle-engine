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

{ Rendering with occlusion query.
  @exclude Internal unit for CastleScene. }
unit CastleSceneInternalOcclusion;

{$I castleconf.inc}

interface

uses
  CastleVectors, CastleSceneCore, CastleSceneInternalShape,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleGLUtils, CastleRenderContext, CastleFrustum, CastleGLShaders,
  CastleBoxes, CastleTransform, CastleRenderPrimitives;

type
  TShapeProcedure = procedure (const Shape: TGLShape) of object;

  TOcclusionQueryUtilsRenderer = class
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
    ModelViewProjectionMatrix: TMatrix4;
    procedure GLContextClose;
    procedure OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
  end;

  TSimpleOcclusionQueryRenderer = class
  strict private
    Scene: TCastleSceneCore;
    Utils: TOcclusionQueryUtilsRenderer;
  public
    constructor Create(const AScene: TCastleSceneCore;
      const AUtils: TOcclusionQueryUtilsRenderer);
    procedure Render(const Shape: TGLShape; const RenderShape: TShapeProcedure;
      const Params: TRenderParams);
  end;

  THierarchicalOcclusionQueryRenderer = class
  private
    FrameId: Cardinal;
    Scene: TCastleSceneCore;
    Utils: TOcclusionQueryUtilsRenderer;
  public
    constructor Create(const AScene: TCastleSceneCore;
      const AUtils: TOcclusionQueryUtilsRenderer);
    procedure Render(const RenderShape: TShapeProcedure;
      const Params: TRenderParams;
      const RenderCameraPosition: TVector3);
    function WasLastVisible(const Shape: TGLShape): boolean;
  end;

implementation

uses SysUtils,
  CastleClassUtils, CastleInternalShapeOctree, CastleRenderOptions,
  CastleUtils;

{ TOcclusionQueryUtilsRenderer ------------------------------------------------- }

procedure TOcclusionQueryUtilsRenderer.GLContextOpen;
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

procedure TOcclusionQueryUtilsRenderer.GLContextClose;
begin
  FreeAndNil(RenderBox);
end;

procedure TOcclusionQueryUtilsRenderer.OcclusionBoxStateBegin;
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

procedure TOcclusionQueryUtilsRenderer.OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
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

procedure TOcclusionQueryUtilsRenderer.DrawBox(const Box: TBox3D);
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

  RenderBox.ModelViewProjection := ModelViewProjectionMatrix;
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

{ TSimpleOcclusionQueryRenderer ---------------------------------------------- }

const
  QueryTarget =
    {$ifndef OpenGLES} GL_SAMPLES_PASSED
    { Following https://registry.khronos.org/OpenGL-Refpages/es3/html/glBeginQuery.xhtml
      it seems better to use GL_ANY_SAMPLES_PASSED_CONSERVATIVE: it may sometimes return
      that object is visible when it is not, but it is faster. }
    {$else} GL_ANY_SAMPLES_PASSED_CONSERVATIVE
    {$endif};

constructor TSimpleOcclusionQueryRenderer.Create(const AScene: TCastleSceneCore;
  const AUtils: TOcclusionQueryUtilsRenderer);
begin
  inherited Create;
  Scene := AScene;
  Utils := AUtils;
end;

procedure TSimpleOcclusionQueryRenderer.Render(const Shape: TGLShape;
   const RenderShape: TShapeProcedure; const Params: TRenderParams);
var
  SampleCount: TGLuint;
begin
  Assert(Shape.OcclusionQueryId <> 0);
  if Shape.OcclusionQueryAsked then
    glGetQueryObjectuiv(Shape.OcclusionQueryId, GL_QUERY_RESULT,
      @SampleCount) else
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
      RenderShape(Shape);
    end else
    begin
      { Object was not visible in the last frame.
        In this frame, only render it's bounding box, to test
        occlusion query. This is the speedup of using occlusion query:
        we render only bbox. }

      Utils.DrawBox(Shape.BoundingBox);
      if (Params.InternalPass = 0) and not Scene.ExcludeFromStatistics then
        Inc(Params.Statistics.BoxesOcclusionQueriedCount);
    end;

  if Params.StencilTest = 0 then
  begin
    glEndQuery(QueryTarget);
    Shape.OcclusionQueryAsked := true;
  end;
end;

{ THierarchicalOcclusionQueryRenderer ---------------------------------------- }

constructor THierarchicalOcclusionQueryRenderer.Create(
  const AScene: TCastleSceneCore;
  const AUtils: TOcclusionQueryUtilsRenderer);
begin
  inherited Create;
  Scene := AScene;
  Utils := AUtils;
end;

procedure THierarchicalOcclusionQueryRenderer.Render(
  const RenderShape: TShapeProcedure;
  const Params: TRenderParams;
  const RenderCameraPosition: TVector3);
var
  { Stack of TShapeOctreeNode.

    Although queue would also work not so bad, stack is better.
    The idea is that it should try to keep front-to-back order,
    assuming that Node.PushChildren* keeps this order.
    Stack gives more chance to process front shapes first. }
  TraversalStack: TCastleObjectStack;

  procedure TraverseNode(Node: TShapeOctreeNode);
  var
    I: Integer;
    Shape: TGLShape;
  begin
    if Node.IsLeaf then
    begin
      { Render all shapes within this leaf, taking care to render
        shape only once within this frame (FrameId is useful here). }
      for I := 0 to Node.ItemsIndices.Count - 1 do
      begin
        Shape := TGLShape(Scene.InternalOctreeRendering.ShapesList[Node.ItemsIndices.List^[I]]);
        if Shape.RenderedFrameId <> FrameId then
        begin
          RenderShape(Shape);
          Shape.RenderedFrameId := FrameId;
        end;
      end;
    end else
    begin
      { Push Node children onto TraversalStack.
        We want to Pop them front-first, so (since this is a stack)
        we want to push back first. }
      Node.PushChildrenBackToFront(TraversalStack, RenderCameraPosition);
    end;
  end;

  procedure PullUpVisibility(Node: TShapeOctreeNode);
  begin
    while not Node.Visible do
    begin
      Node.Visible := true;
      Node := Node.ParentNode;
      if Node = nil then Break;
    end;
  end;

  procedure RenderLeafNodeVolume(Node: TShapeOctreeNode);
  var
    I: Integer;
    Shape: TGLShape;
    Box: TBox3D;
  begin
    { How to render bounding volume of leaf for occlusion query?

      - Simple version is just to render Node.Box. But this may be
        much greater than actual box of shapes inside, Box of our
        octree node is not adjusted to be tight.

      - Another version is to render boxes of all shapes within this leaf.
        This is much tighter than Node.Box, and results in much less
        shapes quialified as visible. (See e.g. bzwgen city view behind
        building 1 when trying to walk towards the city center.)
        Unfortunately, this produces really a lot of boxes, so the
        overhead of drawing glDrawBox3DSimple becomes large then.

      - Compromise: calculate tight bounding box here, and use it.
        Works best: number of both visible shapes and cull boxes
        is small.

      Note that we can render here boxes of only non-rendered shapes,
      that's Ok and may actually speed up. }
    Box := TBox3D.Empty;

    for I := 0 to Node.ItemsIndices.Count - 1 do
    begin
      Shape := TGLShape(Scene.InternalOctreeRendering.ShapesList[Node.ItemsIndices.List^[I]]);
      if Shape.RenderedFrameId <> FrameId then
        Box.Include(Shape.BoundingBox);
    end;

    Utils.DrawBox(Box);
    if (Params.InternalPass = 0) and not Scene.ExcludeFromStatistics then
      Inc(Params.Statistics.BoxesOcclusionQueriedCount);
  end;

const
  VisibilityThreshold = 0;
{ $define VISIBILITY_KEEP_FRAMES}
{$ifdef VISIBILITY_KEEP_FRAMES}
  VisibilityKeepFrames = 10;
{$endif}
var
  { queue of TOcclusionQuery }
  QueryQueue: TCastleObjectQueue;
  Q: TOcclusionQuery;
  Node: TShapeOctreeNode;
  WasVisible, LeafOrWasInvisible: boolean;
begin
  {$include norqcheckbegin.inc}
  Inc(FrameId);
  {$include norqcheckend.inc}

  TraversalStack := TCastleObjectStack.Create;
  TraversalStack.Capacity := Scene.InternalOctreeRendering.ShapesList.Count;

  QueryQueue := TCastleObjectQueue.Create;
  QueryQueue.Capacity := Scene.InternalOctreeRendering.ShapesList.Count;

  try
    TraversalStack.Push(Scene.InternalOctreeRendering.TreeRoot);

    repeat
      if (QueryQueue.Count <> 0) and
         ( (TOcclusionQuery(QueryQueue.Peek).Available) or
           (TraversalStack.Count = 0) ) then
      begin
        Q := TOcclusionQuery(QueryQueue.Pop);
        if Q.GetResult > VisibilityThreshold then
        begin
          PullUpVisibility(Q.Node);
          TraverseNode(Q.Node);
        end;
        FreeAndNil(Q);
      end;

      if TraversalStack.Count <> 0 then
      begin
        Node := TShapeOctreeNode(TraversalStack.Pop);
        if (Params.Frustum = nil) or
           Node.FrustumCollisionPossible(Params.Frustum^) then
        begin
          {$ifdef VISIBILITY_KEEP_FRAMES}
          { There was a resigned idea below (maybe useful later) to do
            "or (Node.Depth >= 5)", to assume visible = true below some
            octree depth. }

          if (Node.Visible and (Node.LastVisitedFrameId >= FrameId - VisibilityKeepFrames)) then
          begin
            { Visible somewhere during VisibilityKeepFrames.
              Just assume it's still visible.
              (This is the optimization described in 6.6.4
              "Conservative Visibility Testing") }
            TraverseNode(Node);
          end else
          {$endif VISIBILITY_KEEP_FRAMES}
          begin
            WasVisible := Node.Visible and (Node.LastVisitedFrameId = FrameId - 1);
            LeafOrWasInvisible := (not WasVisible) or Node.IsLeaf;

            Node.Visible := false;
            Node.LastVisitedFrameId := FrameId;

            { Original logic goes like:

                if LeafOrWasInvisible then
                  Add query with Node.Box;
                if WasVisible then
                  TraverseNode(Node);

              But this is not optimal: it would always query using bounding
              boxes. Even for the case when we have a visible leaf,
              then the above version would query using box of this leaf
              and then render this leaf.
              But in this case we can query using actual geometry.

              So a modification is to do

                if LeafOrWasInvisible then
                begin
                  if Leaf and WasVisible then
                    Add query for Node and render the leaf else
                    Add query with Node.Box;
                end else
                if WasVisible then
                  TraverseNode(Node);

              This exhausts all possibilities, since if
              LeafOrWasInvisible and WasVisible then only leaf nodes
              could satisfy this.

              There's additional note about this:
              rendering inside TraverseNode may render
              only part of the leaf's items (or even none at all).
              This is needed (although in original paper they write
              about rendering single shape there, unline my many-shapes-in-leaf
              approach, but still they have to safeguard against rendering
              the same node many times, since visible leaf confirmed to
              be visible may be passed twice to Render).

              But this means that object may be classified as invisible
              (because it didn't have any unrendered shapes), while in fact
              it's visible. That's not a problem, since we check our
              query in the next frame, and the object will be found
              then visible again (or again invisible if other leafs
              will render it's shapes, but then it's not a problem). }

            if LeafOrWasInvisible then
            begin
              Q := TOcclusionQuery.Create;
              Q.Node := Node;

              glBeginQuery(QueryTarget, Q.Id);
                if Node.IsLeaf and WasVisible then
                  TraverseNode(Node) else
                if Node.IsLeaf then
                  { Leaf nodes have optimized version of rendering their
                    bounding volume for occlusion query. }
                  RenderLeafNodeVolume(Node) else
                begin
                  Utils.DrawBox(Node.Box);
                  if (Params.InternalPass = 0) and not Scene.ExcludeFromStatistics then
                    Inc(Params.Statistics.BoxesOcclusionQueriedCount);
                end;
              glEndQuery(QueryTarget);

              QueryQueue.Push(Q);
            end else
            if WasVisible then
              TraverseNode(Node);
          end;
        end;
      end;

    until (TraversalStack.Count = 0) and (QueryQueue.Count = 0);
  finally
    FreeAndNil(TraversalStack);
    FreeAndNil(QueryQueue);
  end;
end;

function THierarchicalOcclusionQueryRenderer.WasLastVisible(const Shape: TGLShape): boolean;
begin
  Result := Shape.RenderedFrameId = FrameId
end;

end.
