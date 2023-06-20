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

{ Render shapes, possibly coming from different TCastleScene instances. }
unit CastleInternalShapesRenderer;

{ TODO:
  - what happens with background (skybox) rendering?
}

{$I castleconf.inc}

interface

uses CastleVectors, CastleSceneInternalShape, CastleRenderOptions,
  CastleSceneInternalOcclusion, CastleSceneInternalBlending,
  CastleInternalBatchShapes, CastleInternalRenderer, CastleTransform,
  X3DNodes, CastleShapes;

type
  { Collect shapes, possibly coming from different TCastleScene instances,
    to render them together using TShapesRenderer. }
  TShapesCollector = class
  private
    FCollected: TCollectedShapeList;
  public
    constructor Create;
    destructor Destroy; override;

    { Clear shapes to render. }
    procedure Clear;

    { Add a shape to render. }
    procedure Add(const Shape: TGLShape; const RenderOptions: TCastleRenderOptions;
      const SceneTransform: TMatrix4);
  end;

  { Render collected shapes.
    Applies cross-scene shape optimizations and features,
    like dynamic batching (so shapes from different TCastleScene instances
    may be batched too),
    and sorting shapes for blending (so shapes from different TCastleScene instances
    are sorted with each other). }
  TShapesRenderer = class
  strict private
    OcclusionQueryUtilsRenderer: TOcclusionQueryUtilsRenderer;
    SimpleOcclusionQueryRenderer: TSimpleOcclusionQueryRenderer;
    HierarchicalOcclusionQueryRenderer: THierarchicalOcclusionQueryRenderer;
    BlendingRenderer: TBlendingRenderer;
    FRenderer: TGLRenderer;
    FBatching: TBatchShapes;

    { Checks DynamicBatching and TODO: not occlusion query. }
    function ReallyDynamicBatching: Boolean;

    { Created on demand TBatchShapes instance.
      Use only if ReallyDynamicBatching to not create needlessly. }
    function Batching: TBatchShapes;

    { Like LightRender, additionally turn off lights that are not
      supposed to light in the shadow (for shadow volumes).
      This simply turns LightOn to @false if the light has
      shadowVolumes = TRUE (see
      [https://castle-engine.io/x3d_extensions.php#section_ext_shadows]).

      It's useful to pass this as LightRenderEvent to @link(Render)
      when you use shadow algorithm that requires
      you to make a first pass rendering the scene all shadowed. }
    procedure LightRenderInShadow(const Shape: TShape;
      const Light: TLightInstance;
      const IsGlobalLight: Boolean; var LightOn: boolean);

    { Turn off global lights that are duplicated in current scene.
      This way we render our own lights through SceneLights at each shape,
      not through GlobalLights,
      and so they work regardless of TCastleScene.CastGlobalLights
      and RenderOptions.ReceiveGlobalLights,
      and are controled by RenderOptions.ReceiveSceneLights. }
    procedure LightRender(const Shape: TShape;
      const Light: TLightInstance;
      const IsGlobalLight: Boolean; var LightOn: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure PrepareResources;
    procedure GLContextClose;

    { Shapes can use this to initialize their resources.

      TODO: This is hack, association shape->Renderer should not be necessary.
      Shape should store its stuff in GLContextCache and have references to it,
      and be independent from Renderer.
      This property should not be public then. }
    property Renderer: TGLRenderer read FRenderer;

    { Render all given shapes.

      Note: Params.Transform and Params.TransformIdentity may be ignored
      by this routine.
      They should be set to indicate identity.
      They are meaningless here: we have scene transformation in each collected
      shape.

      TODO: Make sure they are ignored, assert they are identity.

      TODO: Split TRenderParams into stuff needed at collection,
      and needed at rendering collecting shapes.
      Transform and Params.TransformIdentity should be only in former. }
    procedure Render(const Shapes: TShapesCollector;
      const Params: TRenderParams; const BlendingSort: TBlendingSort);
  end;

implementation

uses SysUtils,
  CastleScene;

{ TShapesCollector ----------------------------------------------------------- }

constructor TShapesCollector.Create;
begin
  inherited;
  FCollected := TCollectedShapeList.Create(true);
end;

destructor TShapesCollector.Destroy;
begin
  FreeAndNil(FCollected);
  inherited;
end;

procedure TShapesCollector.Clear;
begin
  FCollected.Clear;
end;

procedure TShapesCollector.Add(const Shape: TGLShape;
  const RenderOptions: TCastleRenderOptions;
  const SceneTransform: TMatrix4);
var
  NewCollected: TCollectedShape;
begin
  NewCollected := TCollectedShape.Create;
  NewCollected.Shape := Shape;
  NewCollected.RenderOptions := RenderOptions;
  NewCollected.SceneTransform := SceneTransform;
  FCollected.Add(NewCollected);
end;

{ TShapesRenderer ----------------------------------------------------------- }

constructor TShapesRenderer.Create;
begin
  inherited;
  OcclusionQueryUtilsRenderer := TOcclusionQueryUtilsRenderer.Create;
  SimpleOcclusionQueryRenderer := TSimpleOcclusionQueryRenderer.Create(
    {Self} nil { TODO occlusion query broken }, OcclusionQueryUtilsRenderer);
  HierarchicalOcclusionQueryRenderer := THierarchicalOcclusionQueryRenderer.Create(
    {Self} nil { TODO occlusion query broken }, OcclusionQueryUtilsRenderer);
  BlendingRenderer := TBlendingRenderer.Create;
  FRenderer := TGLRenderer.Create(nil);
end;

destructor TShapesRenderer.Destroy;
begin
  FreeAndNil(HierarchicalOcclusionQueryRenderer);
  FreeAndNil(SimpleOcclusionQueryRenderer);
  FreeAndNil(OcclusionQueryUtilsRenderer);
  FreeAndNil(BlendingRenderer);
  FreeAndNil(FRenderer);
  FreeAndNil(FBatching);
  inherited;
end;

procedure TShapesRenderer.PrepareResources;

  { Call TGLShape.PrepareResources for shapes in "batching pool". }
  procedure BatchingShapesPrepareResources;
  var
    I: Integer;
    DummyRenderOptions: TCastleRenderOptions;
    NeedsRenderOptions: Boolean;
  begin
    // TODO: should not be required here?
    DummyRenderOptions := TCastleRenderOptions.Create(nil);
    try
      for I := 0 to Batching.PoolShapesCount - 1 do
      begin
        Assert(Batching.PoolShapes[I] <> nil);
        NeedsRenderOptions := Batching.PoolShapes[I].OverrideRenderOptions = nil;
        { We need to set OverrideRenderOptions,
          otherwise the PoolShapes[I] would not have any RenderOptions
          instance, which is necessary to be prepared. }
        if NeedsRenderOptions then
          Batching.PoolShapes[I].OverrideRenderOptions := DummyRenderOptions;
        Batching.PoolShapes[I].PrepareResources(FRenderer);
        if NeedsRenderOptions then
          Batching.PoolShapes[I].OverrideRenderOptions := nil;
      end;
    finally FreeAndNil(DummyRenderOptions) end;
  end;

  { Simulate rendering for shapes in "batching pool". }
  procedure BatchingShapesRender;
  var
    I: Integer;
    DummyStatistics: TRenderStatistics;
    DummyCamera: TRenderingCamera;
    DummyRenderOptions: TCastleRenderOptions;
    SavedRenderMode: TGLRenderer.TRenderMode;
    Shape: TGLShape;
  begin
    // TODO: should not be required for rendering here?
    DummyRenderOptions := TCastleRenderOptions.Create(nil);
    try
      SavedRenderMode := Renderer.RenderMode;
      Renderer.RenderMode := rmPrepareRenderSelf;

      DummyCamera := TRenderingCamera.Create;
      try
        DummyCamera.FromMatrix(TVector3.Zero,
          TMatrix4.Identity, TMatrix4.Identity, TMatrix4.Identity);

        Renderer.RenderBegin(nil, DummyCamera, nil, 0, 0, 0, @DummyStatistics);

        for I := 0 to Batching.PoolShapesCount - 1 do
        begin
          Shape := Batching.PoolShapes[I];
          TGLShape(Shape).Fog := nil;
          Renderer.RenderShape(TGLShape(Shape), DummyRenderOptions, TMatrix4.Identity);
        end;

        Renderer.RenderEnd;
      finally FreeAndNil(DummyCamera) end;

      Renderer.RenderMode := SavedRenderMode; // restore Renderer.RenderMode
    finally FreeAndNil(DummyRenderOptions) end;
  end;

begin
  if ReallyDynamicBatching then
  begin
    BatchingShapesPrepareResources;
    BatchingShapesRender;
  end;
end;

procedure TShapesRenderer.GLContextClose;
begin
  if Renderer <> nil then
    Renderer.UnprepareAll;

  if OcclusionQueryUtilsRenderer <> nil then
    OcclusionQueryUtilsRenderer.GLContextClose;

  if FBatching <> nil then
    FBatching.GLContextClose;
end;

function TShapesRenderer.Batching: TBatchShapes;
begin
  if FBatching = nil then
    FBatching := TBatchShapes.Create;
  Result := FBatching;
end;

procedure TShapesRenderer.LightRenderInShadow(const Shape: TShape;
  const Light: TLightInstance;
  const IsGlobalLight: Boolean; var LightOn: boolean);
begin
  if Light.Node.FdShadowVolumes.Value then
    LightOn := false;
  LightRender(Shape, Light, IsGlobalLight, LightOn);
end;

procedure TShapesRenderer.LightRender(const Shape: TShape;
  const Light: TLightInstance;
  const IsGlobalLight: Boolean; var LightOn: boolean);
begin
  if IsGlobalLight and
      (*Do not filter out headlight nodes, even if they belong to current scene.
        Headlight nodes are always considered "global lights" and are not present
        on our GlobalLights list, so we don't want to filter them out here.

        Testcase: castle-game, with "Tower" level that defines in basic_castle_final.x3dv
        headlight like this:

          NavigationInfo {
            headlight TRUE
            headlightNode DirectionalLight {
              ...
            }
          }
      *)
      (not Light.Node.InternalHeadlight) and
      (Light.Node.Scene = Shape.ParentScene) then
    LightOn := false;
end;

procedure TShapesRenderer.Render(const Shapes: TShapesCollector;
  const Params: TRenderParams; const BlendingSort: TBlendingSort);

  procedure RenderShape_NoTests(const CollectedShape: TCollectedShape);
  var
    Shape: TGLShape;
  begin
    { TODO: ignore ExcludeFromStatistics now (we could access them
      from Shape.ParaneScene...
      but this would be bad for batching.)
      Just remove ExcludeFromStatistics? }
    if (Params.InternalPass = 0) {and not ExcludeFromStatistics} then
      Inc(Params.Statistics.ShapesRendered);

    Shape := CollectedShape.Shape;

    BlendingRenderer.BeforeRenderShape(Shape);
    Renderer.RenderShape(Shape,
      CollectedShape.RenderOptions, CollectedShape.SceneTransform);
  end;

  procedure BatchingCommit;
  var
    Shape: TCollectedShape;
  begin
    if ReallyDynamicBatching then
    begin
      Batching.Commit;
      for Shape in Batching.Collected do
      begin
        Shape.Shape.PrepareResources(FRenderer); // otherwise, shapes from batching FPool would never have PrepareResources called?
        RenderShape_NoTests(Shape);
      end;
      Batching.FreeCollected;
    end;
  end;

var
  LightRenderEvent: TLightRenderEvent;
  CollectedShape: TCollectedShape;
begin
  { TODO: This optimization should be useless, remove.
    For now it helps with hack that access 0th scene RenderOptions below. }
  if Shapes.FCollected.Count = 0 then
    Exit;

  if Params.InShadow then
    LightRenderEvent := {$ifdef FPC}@{$endif}LightRenderInShadow
  else
    LightRenderEvent := {$ifdef FPC}@{$endif}LightRender;

  // TODO: occlusion query at viewport controlled (and sorted)?
  { update OcclusionQueryUtilsRenderer.ModelViewProjectionMatrix if necessary }
  // if ReallyAnyOcclusionQuery(RenderOptions) then
  // begin
  //   OcclusionQueryUtilsRenderer.ModelViewProjectionMatrix :=
  //     RenderContext.ProjectionMatrix * Render_ModelView;
  //   //OcclusionQueryUtilsRenderer.ModelViewProjectionMatrixChanged := true; // not needed anymore
  // end;

  { Initialize Batching.
    PreserveShapeOrder may be overridden to true below. }
  if ReallyDynamicBatching then
  begin
    Batching.PreserveShapeOrder := false;
    Assert(Batching.Collected.Count = 0);
  end;

  if Params.Transparent then
  begin
    { We'll draw partially transparent objects now,
      only from scenes with RenderOptions.Blending. }

    if ReallyDynamicBatching then
      Batching.PreserveShapeOrder := true;

    { TODO: The sorting is repeated at every render call.
      This is not optimal in case of 2D, where it would be more efficient
      to only call @link(SortBackToFront2D) when necessary, e.g. when adding/removing
      objects from the world.

      To avoid this overhead, just leave this property at bsNone
      and call @link(SortBackToFront2D) when necessary.
    }
    Shapes.FCollected.SortBackToFront(Params.RenderingCamera.Position,
      BlendingSort);

    BlendingRenderer.RenderBegin;
  end;

  Renderer.RenderBegin(Params.GlobalLights as TLightInstancesList,
    Params.RenderingCamera,
    LightRenderEvent, Params.InternalPass,
    {InternalScenePass}{TODO}0, Params.UserPass, @Params.Statistics);
  try
    for CollectedShape in Shapes.FCollected do
    begin
      if not (ReallyDynamicBatching and Batching.Collect(CollectedShape)) then
        RenderShape_NoTests(CollectedShape);
    end;

    BatchingCommit;

    { This must be called after BatchingCommit,
      since BatchingCommit may render some shapes }
    if Params.Transparent then
      BlendingRenderer.RenderEnd;

    { As each RenderShape_SomeTests inside could set OcclusionBoxState,
      be sure to restore state now. }
    OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd(true);
  finally Renderer.RenderEnd end;
end;

function TShapesRenderer.ReallyDynamicBatching: boolean;
begin
  Result := DynamicBatching; // TODO: nothing more for now, should check occlusion culling though
end;

end.
