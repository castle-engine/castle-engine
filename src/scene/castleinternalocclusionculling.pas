{
  Copyright 2003-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleGLUtils, CastleRenderContext, CastleFrustum, CastleGLShaders,
  CastleBoxes, CastleTransform, CastleRenderPrimitives, CastleRenderOptions,
  CastleInternalRHI;

type
  TShapeRenderEvent = procedure (const Shape: TCollectedShape;
    const Params: TRenderParams; const PassParams: TRenderOnePassParams) of object;

  TOcclusionCullingUtilsRenderer = class
  strict private
    RenderBox: TCastleRenderUnlitMesh;
    FRHIBoxVertexBuffer: TRHIBuffer;
    FRHIBoxIndexBuffer: TRHIBuffer;
    OcclusionBoxState: boolean;
    SavedDepthBufferUpdate: Boolean;
    SavedColorChannels: TColorChannels;
    SavedCullFace: Boolean;
    function GetRHIBoxVertexBuffer: TRHIBuffer;
    function GetRHIBoxIndexBuffer: TRHIBuffer;
    procedure GLContextOpen;
    procedure OcclusionBoxStateBegin;
  private
    procedure DrawBox(const Box: TBox3D);
  public
    ProjectionMatrix: TMatrix4;
    CameraMatrix: TMatrix4;
    SceneMatrix: TMatrix4;
    destructor Destroy; override;
    procedure GLContextClose;
    procedure OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
    property RHIBoxVertexBuffer: TRHIBuffer read GetRHIBoxVertexBuffer;
    property RHIBoxIndexBuffer: TRHIBuffer read GetRHIBoxIndexBuffer;
    property RenderBoxMesh: TCastleRenderUnlitMesh read RenderBox;

  end;

  TOcclusionCullingRenderer = class
  strict private
    FUtils: TOcclusionCullingUtilsRenderer;
    FRHIQueryPools: array[0..MaxFramesInFlight - 1] of array of TRHIQueryPool;
    FRHIQueryPageCapacity: Cardinal;
    FNextRHIQuerySlot: Cardinal;
    FRHIQueriesIssuedCount: Cardinal;
    FRHIQueriesHitCount: Cardinal;
    function CurrentRHIQueryBank: Cardinal;
    procedure EnsureRHIQueryPoolPage(const ABank, ARequiredPage: Cardinal);
    function GetRHIQueryPoolCount: Cardinal;
    function AllocateRHIQuerySlot: Cardinal;
    function GetRHIQueryPoolAndLocalIndex(const ABank, ASlot: Cardinal;
      out ALocalIndex: Cardinal): TRHIQueryPool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GLContextClose;
    property Utils: TOcclusionCullingUtilsRenderer read FUtils;
    property RHIQueryPageCapacity: Cardinal read FRHIQueryPageCapacity write FRHIQueryPageCapacity;
    property RHIQueryPoolCount: Cardinal read GetRHIQueryPoolCount;
    property AllocatedRHIQuerySlots: Cardinal read FNextRHIQuerySlot;
    property RHIQueriesIssuedCount: Cardinal read FRHIQueriesIssuedCount;
    property RHIQueriesHitCount: Cardinal read FRHIQueriesHitCount;
    function GetRHIQueryPool(const APageIndex: Cardinal): TRHIQueryPool;
    procedure Render(const CollectedShape: TCollectedShape;
      const Params: TRenderParams; const PassParams: TRenderOnePassParams;
      const RenderShape: TShapeRenderEvent);
  end;

implementation

uses SysUtils, Math,
  CastleClassUtils, CastleInternalShapeOctree, CastleInternalGLUtils,
  CastleUtils, CastleTimeUtils, CastleLog, CastleApplicationProperties;

{ TOcclusionCullingUtilsRenderer ------------------------------------------------- }

destructor TOcclusionCullingUtilsRenderer.Destroy;
begin
  GLContextClose;
  inherited;
end;

function TOcclusionCullingUtilsRenderer.GetRHIBoxVertexBuffer: TRHIBuffer;
begin
  if (RenderBox <> nil) and (RenderBox.RHIVertexBuffer <> nil) then
    Result := RenderBox.RHIVertexBuffer
  else
    Result := FRHIBoxVertexBuffer;
end;

function TOcclusionCullingUtilsRenderer.GetRHIBoxIndexBuffer: TRHIBuffer;
begin
  if (RenderBox <> nil) and (RenderBox.RHIIndexBuffer <> nil) then
    Result := RenderBox.RHIIndexBuffer
  else
    Result := FRHIBoxIndexBuffer;
end;

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
  if RHIDevice <> nil then
  begin
    if FRHIBoxIndexBuffer = nil then
      FRHIBoxIndexBuffer := RHIDevice.CreateBuffer(
        TRHIBufferDesc.Create(SizeOf(Indexes), [buIndex], muCpuToGpu, 'OcclusionBoxIBO'),
        @Indexes[0]);
    if FRHIBoxVertexBuffer = nil then
      FRHIBoxVertexBuffer := RHIDevice.CreateBuffer(
        TRHIBufferDesc.Create(8 * SizeOf(TVector4), [buVertex], muCpuToGpu, 'OcclusionBoxVBO'), nil);
    if RenderBox = nil then
    begin
      RenderBox := TCastleRenderUnlitMesh.Create(false);
      RenderBox.SetIndexes(Indexes);
    end;

  end else
  begin
    RenderBox := TCastleRenderUnlitMesh.Create(false);
    RenderBox.SetIndexes(Indexes);
  end;
end;

procedure TOcclusionCullingUtilsRenderer.GLContextClose;
begin
  FreeAndNil(RenderBox);
  FreeAndNil(FRHIBoxVertexBuffer);
  FreeAndNil(FRHIBoxIndexBuffer);
end;

procedure TOcclusionCullingUtilsRenderer.OcclusionBoxStateBegin;
begin
  if not OcclusionBoxState then
  begin
    if RHIDevice <> nil then
    begin
      if FRHIBoxIndexBuffer = nil then
        GLContextOpen;
    end else
    begin
      if RenderBox = nil then
        GLContextOpen;
    end;

    if RenderContext <> nil then
    begin
      SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
      SavedColorChannels := RenderContext.ColorChannels;
      SavedCullFace := RenderContext.CullFace;

      if (RHIDevice <> nil) or ApplicationProperties.IsGLContextOpen then
      begin
        RenderContext.DepthBufferUpdate := false;
        RenderContext.ColorChannels := [];
        RenderContext.CullFace := false;

        { Do not alpha test.
          This also means that texture enabled for fixed-function will be meaningless,
          it will not affect occlusion result, which is good.

          Note that we don't care to save/restore this state,
          as each state will reenable alpha test by RenderContext.FixedFunctionAlphaTestEnable
          if necessary. }
        if ApplicationProperties.IsGLContextOpen then
          RenderContext.FixedFunctionAlphaTestDisable;
      end;
    end;

    if ActiveCommandList <> nil then
    begin
      ActiveCommandList.SetDepthWriteEnable(False);
      ActiveCommandList.SetCullMode(cmNone);
    end;

    OcclusionBoxState := true;
  end;
end;

procedure TOcclusionCullingUtilsRenderer.OcclusionBoxStateEnd(const RestoreDefaults: Boolean);
begin
  if OcclusionBoxState then
  begin
    if (RenderContext <> nil) and ((RHIDevice <> nil) or ApplicationProperties.IsGLContextOpen) then
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
    end;

    if ActiveCommandList <> nil then
    begin
      if RestoreDefaults then
      begin
        ActiveCommandList.SetDepthWriteEnable(True);
        ActiveCommandList.SetCullMode(cmNone);
      end else
      begin
        ActiveCommandList.SetDepthWriteEnable(SavedDepthBufferUpdate);
        if SavedCullFace then
          ActiveCommandList.SetCullMode(cmBack)
        else
          ActiveCommandList.SetCullMode(cmNone);
      end;
    end;

    OcclusionBoxState := false;
  end;
end;

procedure TOcclusionCullingUtilsRenderer.DrawBox(const Box: TBox3D);
var
  Verts: array [0..7] of TVector4;
  Mapped: Pointer;
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

  if RenderBox <> nil then
  begin
    RenderBox.ModelViewProjection := ProjectionMatrix * CameraMatrix * SceneMatrix;
    RenderBox.SetVertexes(Verts, true);
    RenderBox.Render(pmTriangles);
    Exit;
  end;

  if RHIDevice <> nil then
  begin
    if FRHIBoxVertexBuffer <> nil then
    begin
      Mapped := FRHIBoxVertexBuffer.Map;
      if Mapped <> nil then
      begin
        Move(Verts[0], Mapped^, SizeOf(Verts));
        FRHIBoxVertexBuffer.Unmap;
      end;
    end;
    if (ActiveCommandList <> nil) and (FRHIBoxVertexBuffer <> nil) and (FRHIBoxIndexBuffer <> nil) then
    begin
      ActiveCommandList.BindVertexBuffers(0, [FRHIBoxVertexBuffer], [0]);
      ActiveCommandList.BindIndexBuffer(FRHIBoxIndexBuffer, 0, itUInt16);
      ActiveCommandList.DrawIndexed(36, 1, 0, 0, 0);
    end;
  end else
  begin
    RenderBox.ModelViewProjection := ProjectionMatrix * CameraMatrix * SceneMatrix;
    RenderBox.SetVertexes(Verts, true);
    RenderBox.Render(pmTriangles);
  end;
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
  FRHIQueryPageCapacity := 256;
end;

destructor TOcclusionCullingRenderer.Destroy;
begin
  GLContextClose;
  FreeAndNil(FUtils);
  inherited;
end;

procedure TOcclusionCullingRenderer.GLContextClose;
var
  Bank, I: Integer;
begin
  if FUtils <> nil then
    FUtils.GLContextClose;
  for Bank := 0 to MaxFramesInFlight - 1 do
  begin
    for I := 0 to High(FRHIQueryPools[Bank]) do
      FreeAndNil(FRHIQueryPools[Bank][I]);
    SetLength(FRHIQueryPools[Bank], 0);
  end;
  FNextRHIQuerySlot := 0;
end;

function TOcclusionCullingRenderer.CurrentRHIQueryBank: Cardinal;
begin
  if RHIDevice <> nil then
    Result := RHIDevice.FrameIndex mod MaxFramesInFlight
  else
    Result := 0;
end;

procedure TOcclusionCullingRenderer.EnsureRHIQueryPoolPage(const ABank, ARequiredPage: Cardinal);
var
  Capacity, PageIdx: Cardinal;
begin
  if (RHIDevice = nil) or (ABank >= MaxFramesInFlight) then
    Exit;
  Capacity := Max(Cardinal(1), FRHIQueryPageCapacity);
  while Cardinal(Length(FRHIQueryPools[ABank])) <= ARequiredPage do
  begin
    PageIdx := Cardinal(Length(FRHIQueryPools[ABank]));
    SetLength(FRHIQueryPools[ABank], PageIdx + 1);
    FRHIQueryPools[ABank][PageIdx] := RHIDevice.CreateQueryPool(
      TRHIQueryPoolDesc.Create(qtOcclusion, Capacity,
        Format('OcclusionCullingQueryPool_Bank%d_Page%d', [ABank, PageIdx])));
  end;
end;

function TOcclusionCullingRenderer.GetRHIQueryPoolCount: Cardinal;
begin
  Result := Cardinal(Length(FRHIQueryPools[0]));
end;

function TOcclusionCullingRenderer.GetRHIQueryPool(const APageIndex: Cardinal): TRHIQueryPool;
begin
  if APageIndex < Cardinal(Length(FRHIQueryPools[0])) then
    Result := FRHIQueryPools[0][APageIndex]
  else
    Result := nil;
end;

function TOcclusionCullingRenderer.AllocateRHIQuerySlot: Cardinal;
var
  Capacity, RequiredPage: Cardinal;
begin
  Inc(FNextRHIQuerySlot);
  Result := FNextRHIQuerySlot; // 1-based slot index

  if RHIDevice = nil then
    Exit;

  Capacity := Max(Cardinal(1), FRHIQueryPageCapacity);
  RequiredPage := (Result - 1) div Capacity;
  EnsureRHIQueryPoolPage(CurrentRHIQueryBank, RequiredPage);
end;

function TOcclusionCullingRenderer.GetRHIQueryPoolAndLocalIndex(
  const ABank, ASlot: Cardinal; out ALocalIndex: Cardinal): TRHIQueryPool;
var
  Capacity, ZeroBased, PageIdx: Cardinal;
begin
  Result := nil;
  ALocalIndex := 0;
  if (ASlot = 0) or (ABank >= MaxFramesInFlight) then
    Exit;
  Capacity := Max(Cardinal(1), FRHIQueryPageCapacity);
  ZeroBased := ASlot - 1;
  PageIdx := ZeroBased div Capacity;
  ALocalIndex := ZeroBased mod Capacity;
  EnsureRHIQueryPoolPage(ABank, PageIdx);
  if PageIdx < Cardinal(Length(FRHIQueryPools[ABank])) then
    Result := FRHIQueryPools[ABank][PageIdx];
end;

procedure TOcclusionCullingRenderer.Render(const CollectedShape: TCollectedShape;
  const Params: TRenderParams; const PassParams: TRenderOnePassParams;
  const RenderShape: TShapeRenderEvent);

  { Read OpenGL(ES) occlusion query result, return if hit > 0 samples
    (was visible). }
  function OcclusionQueryHit(const OcclusionQueryId: TGLQuery): Boolean;
  var
    SampleCount: TGLuint;
  begin
    {$ifdef CASTLE_WEBGL}
    SampleCount := glGetQueryParameter(OcclusionQueryId, GL_QUERY_RESULT);
    {$else}
    glGetQueryObjectuiv(OcclusionQueryId, GL_QUERY_RESULT, @SampleCount);
    {$endif}
    Result := SampleCount > 0;
  end;

  function OcclusionQueryResultAvailable(const OcclusionQueryId: TGLQuery): Boolean;
  begin
    {$ifdef CASTLE_WEBGL}
    { Is the occlusion query result available.

      On the web, asking for GL_QUERY_RESULT is like asking for
      GL_QUERY_RESULT_NO_WAIT on desktop.
      This means that if the result isn't ready, we get 0.

      And it is *never* ready in the same frame. We must wait some more
      (and not issue a new query) until it is ready.

      See
      - web:
        https://www.realtimerendering.com/blog/webgl-2-new-features/
        https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext/getQueryParameter
      - desktop: https://registry.khronos.org/OpenGL-Refpages/gl4/html/glGetQueryObject.xhtml
    }
    Result := glGetQueryParameter(OcclusionQueryId, GL_QUERY_RESULT_AVAILABLE);
    {$else}
    { On non-web (OpenGL, OpenGLES), behave as if GL_QUERY_RESULT_AVAILABLE is true,
      because we ask using GL_QUERY_RESULT which waits for result. }
    Result := true;
    {$endif}
  end;

  { We cannot trust the occlusion query results if the parent scene,
    and so also CollectedShape, occur multiple times within the Viewport
    under a different transformation.
    Making multiple occlusion queries and using only the last one would be wrong.

    Testcase: fps_game with OcclusionCulling - see at trees. }
  function SceneMultipleInstances: Boolean;
  begin
    Result :=
      { Note that Shape.ParentScene is useless when batching (always nil
        for batched shapes)... but luckily, occlusion culling is never used
        together with batching. }
      (CollectedShape.Shape.ParentScene <> nil) and
      (CollectedShape.Shape.ParentScene.InternalWorldReferences > 1);
  end;

const
  { On WebGL, we sometimes get occlusion query results after many frames
    since asking. Ignore them in such case. }
  MaxOcclusionQueryResultAge = 1; // frames
  MaxRHIOcclusionQueryResultAge = MaxFramesInFlight; // double-buffered frame-in-flight banks
var
  Shape: TGLShape;
  WasVisible: Boolean;
  QueryResultAvailable, OcclusionQueryAsk: Boolean;
  QueryResultAge: Int64;
  RHIQueryPool: TRHIQueryPool;
  RHIBank, ReadBank, LocalQueryIdx: Cardinal;
  RHISampleCounts: array[0..0] of UInt64;
begin
  Shape := CollectedShape.Shape;
  RHIBank := 0;

  { Get occlusion query result from previous render.
    Prefer the most recently issued bank (1-frame latency when ready, and matches
    headless SetSimulatedResult tests), falling back to the fence-retired current bank
    when the previous frame is still executing on the GPU. }
  if RHIDevice <> nil then
  begin
    RHIBank := CurrentRHIQueryBank;
    ReadBank := Shape.RHILastAskedBank;
    if (Shape.RHIQuerySlot > 0) and Shape.RHIBankAsked[ReadBank] then
    begin
      RHIQueryPool := GetRHIQueryPoolAndLocalIndex(ReadBank, Shape.RHIQuerySlot, LocalQueryIdx);
      RHISampleCounts[0] := 0;
      QueryResultAvailable := (RHIQueryPool <> nil) and
        RHIQueryPool.GetResults(LocalQueryIdx, 1, RHISampleCounts, False);
      if (not QueryResultAvailable) and (ReadBank <> RHIBank) and Shape.RHIBankAsked[RHIBank] then
      begin
        ReadBank := RHIBank;
        RHIQueryPool := GetRHIQueryPoolAndLocalIndex(ReadBank, Shape.RHIQuerySlot, LocalQueryIdx);
        RHISampleCounts[0] := 0;
        QueryResultAvailable := (RHIQueryPool <> nil) and
          RHIQueryPool.GetResults(LocalQueryIdx, 1, RHISampleCounts, False);
      end;
      if RHIQueryPool <> nil then
      begin
        if not QueryResultAvailable then
          WasVisible := true // assume is visible until GPU finishes query
        else
        begin
          QueryResultAge := TFramesPerSecond.RenderFrameId - Shape.RHIBankAskedFrameId[ReadBank];
          if QueryResultAge > MaxRHIOcclusionQueryResultAge then
            WasVisible := true // assume is visible, query result is too old to trust it
          else
          begin
            WasVisible := RHISampleCounts[0] > 0;
            if WasVisible then
              Inc(FRHIQueriesHitCount);
          end;
        end;
        { Current bank is guaranteed fence-retired by BeginFrame, so we can always
          issue a new query into RHIBank even if PrevBank was still in flight. }
        if ReadBank <> RHIBank then
          QueryResultAvailable := True;
      end else
      begin
        WasVisible := true;
        QueryResultAvailable := true;
      end;
    end else
    begin
      WasVisible := true;
      QueryResultAvailable := true;
    end;
  end else
  if Shape.OcclusionQueryAsked and
     (Shape.OcclusionQueryId <> GLObjectNone) then
  begin
    QueryResultAvailable := OcclusionQueryResultAvailable(Shape.OcclusionQueryId);
    if not QueryResultAvailable then
      WasVisible := true // assume is visible
    else
    begin
      QueryResultAge := TFramesPerSecond.RenderFrameId - Shape.OcclusionQueryAskedFrameId;
      if QueryResultAge > MaxOcclusionQueryResultAge then
        WasVisible := true // assume is visible, query result is too old to trust it
      else
        WasVisible := OcclusionQueryHit(Shape.OcclusionQueryId);
    end;
  end else
  begin
    WasVisible := true; // assume is visible
    QueryResultAvailable := true; // allow issuing a new query below
  end;

  { Render shape, or shape box, possibly making new occlusion query. }
  OcclusionQueryAsk :=
    { Do not do occlusion query (although still use results from previous
      query) if we're within stencil test. This would incorrectly mark some shapes
      as non-visible (just because they don't pass stencil test on any pixel),
      while in fact they should be visible in the very next
      render pass. }
    (not PassParams.InsideStencilTest) and
    (not SceneMultipleInstances) and
    { only issue a new query if the previous one has resolved }
    QueryResultAvailable;

  Shape.OcclusionQueryAsked := Shape.OcclusionQueryAsked or OcclusionQueryAsk;

  if OcclusionQueryAsk then
  begin
    if RHIDevice <> nil then
    begin
      if Shape.RHIQuerySlot = 0 then
        Shape.RHIQuerySlot := AllocateRHIQuerySlot;

      RHIQueryPool := GetRHIQueryPoolAndLocalIndex(RHIBank, Shape.RHIQuerySlot, LocalQueryIdx);
      if (RHIQueryPool <> nil) and (ActiveCommandList <> nil) then
      begin
        ActiveCommandList.ResetQueryPool(RHIQueryPool, LocalQueryIdx, 1);
        ActiveCommandList.BeginQuery(RHIQueryPool, LocalQueryIdx, False);
      end;

      if WasVisible then
      begin
        RenderShape(CollectedShape, Params, PassParams);
      end else
      begin
        Utils.SceneMatrix := CollectedShape.SceneTransform;
        Utils.DrawBox(Shape.BoundingBox);
        if Params.InternalPass = 0 then
          Inc(Params.Statistics.BoxesOcclusionQueriedCount);
      end;

      if (RHIQueryPool <> nil) and (ActiveCommandList <> nil) then
        ActiveCommandList.EndQuery(RHIQueryPool, LocalQueryIdx);

      Inc(FRHIQueriesIssuedCount);
      Shape.RHILastAskedBank := RHIBank;
      Shape.RHIBankAsked[RHIBank] := True;
      Shape.RHIBankAskedFrameId[RHIBank] := TFramesPerSecond.RenderFrameId;
      Shape.OcclusionQueryAskedFrameId := TFramesPerSecond.RenderFrameId;
    end else
    begin
      if Shape.OcclusionQueryId = GLObjectNone then
        Shape.OcclusionQueryId := glCreateQuery();

      glBeginQuery(QueryTarget, Shape.OcclusionQueryId);

      if WasVisible then
      begin
        RenderShape(CollectedShape, Params, PassParams);
      end else
      begin
        { Object was not visible in the last frame.
          In this frame, only render it's bounding box, to test
          occlusion query. This is the speedup of using occlusion query:
          we render only bbox. }

        Utils.SceneMatrix := CollectedShape.SceneTransform;
        Utils.DrawBox(Shape.BoundingBox);
        if Params.InternalPass = 0 then
          Inc(Params.Statistics.BoxesOcclusionQueriedCount);
      end;

      glEndQuery(QueryTarget);

      Shape.OcclusionQueryAskedFrameId := TFramesPerSecond.RenderFrameId;
    end;
  end else
  begin
    if WasVisible then
      RenderShape(CollectedShape, Params, PassParams);
  end;
end;

end.
