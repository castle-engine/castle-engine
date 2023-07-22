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

{$I castleconf.inc}

interface

uses CastleVectors, CastleSceneInternalShape, CastleRenderOptions,
  CastleInternalOcclusionCulling, CastleSceneInternalBlending,
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
      const SceneTransform: TMatrix4; const DepthRange: TDepthRange);
  end;

  { Render collected shapes.
    Applies cross-scene shape optimizations and features,
    like dynamic batching (so shapes from different TCastleScene instances
    may be batched too),
    and sorting shapes for blending (so shapes from different TCastleScene instances
    are sorted with each other). }
  TShapesRenderer = class
  strict private
    FOcclusionCullingRenderer: TOcclusionCullingRenderer;
    FBlendingRenderer: TBlendingRenderer;
    FRenderer: TRenderer;
    FBatching: TBatchShapes;
    FDynamicBatching: Boolean;
    FOcclusionCulling: Boolean;
    FOcclusionSort, FBlendingSort: TShapeSortNoAuto;
    FOnCustomShapeSort: TShapeSortEvent;

    { Checks we need DynamicBatching and we don't use occlusion culling. }
    function EffectiveDynamicBatching: Boolean;

    { Checks we need occlusion culling, and GPU has capability for it. }
    function EffectiveOcclusionCulling: Boolean;

    { Created on demand TBatchShapes instance.
      Use only if EffectiveDynamicBatching to not create needlessly. }
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

    procedure SetOcclusionCulling(const Value: Boolean);

    procedure RenderShape_NoTests(
      const CollectedShape: TCollectedShape; const Params: TRenderParams);
    procedure RenderShape_OcclusionTests(
      const CollectedShape: TCollectedShape; const Params: TRenderParams);
  public
    constructor Create;
    destructor Destroy; override;

    procedure PrepareResources;
    procedure GLContextClose;

    { Shapes can use this to make a "dummy render" to prepare
      for future rendering. }
    property Renderer: TRenderer read FRenderer;

    { Render all given shapes.

      Note: Params.Transform / InverseTransform may be ignored
      by this routine.
      They should be set to indicate identity.
      They are meaningless here: we have scene transformation in each collected
      shape.
      Same for Params.DepthRange (we follow this on shape now),

      TODO: Make sure they are ignored, assert they are identity.

      TODO: Split TRenderParams into stuff needed at collection,
      and needed at rendering collecting shapes.
      Transform / InverseTransform should be only in former. }
    procedure Render(const Shapes: TShapesCollector;
      const Params: TRenderParams);

    { Combine (right before rendering) multiple shapes with a similar appearance into one.
      This can drastically reduce the number of "draw calls",
      making rendering much faster. }
    property DynamicBatching: Boolean
      read FDynamicBatching write FDynamicBatching default false;

    { Use the occlusion culling to optimize the rendering.
      The shapes obscured by other shapes will not be rendered.
      This makes sense when in your view, many shapes are typically obscured by others.

      See the https://castle-engine.io/occlusion_culling
      for details how does this work.

      This is ignored if GPU doesn't support the necessary functionality
      (@link(TGLFeatures.OcclusionQuery)). }
    property OcclusionCulling: boolean
      read FOcclusionCulling write SetOcclusionCulling default false;

    { Sort the opaque shapes when rendering, from front to back.
      @seealso TCastleViewport.OcclusionSort }
    property OcclusionSort: TShapeSortNoAuto
      read FOcclusionSort write FOcclusionSort default sortNone;

    { Sort the blending (partially-transparent) shapes when rendering,
      from back to front.
      @seealso TCastleViewport.BlendingSort }
    property BlendingSort: TShapeSortNoAuto
      read FBlendingSort write FBlendingSort default sortNone;

    { Used to sort shapes, if @link(TCastleViewport.BlendingSort) or
      @link(TCastleViewport.OcclusionSort) indicate sortCustom.
      @seealso TCastleViewport.OnCustomShapeSort }
    property OnCustomShapeSort: TShapeSortEvent
      read FOnCustomShapeSort write FOnCustomShapeSort;
  end;

implementation

uses SysUtils,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleScene, CastleGLUtils, CastleRenderContext, CastleColors, CastleUtils,
  X3DCameraUtils;

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
  const SceneTransform: TMatrix4;
  const DepthRange: TDepthRange);
var
  NewCollected: TCollectedShape;
begin
  NewCollected := TCollectedShape.Create;
  NewCollected.Shape := Shape;
  NewCollected.RenderOptions := RenderOptions;
  NewCollected.SceneTransform := SceneTransform;
  NewCollected.DepthRange := DepthRange;
  FCollected.Add(NewCollected);
end;

{ TShapesRenderer ----------------------------------------------------------- }

constructor TShapesRenderer.Create;
begin
  inherited;
  FOcclusionCullingRenderer := TOcclusionCullingRenderer.Create;
  FBlendingRenderer := TBlendingRenderer.Create;
  FRenderer := TRenderer.Create(nil);
  FOcclusionSort := sortNone;
  FBlendingSort := sortNone;
end;

destructor TShapesRenderer.Destroy;
begin
  FreeAndNil(FOcclusionCullingRenderer);
  FreeAndNil(FBlendingRenderer);
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
        Batching.PoolShapes[I].PrepareResources;
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
    SavedRenderMode: TRenderer.TRenderMode;
    Shape: TGLShape;
  begin
    // TODO: should not be required for rendering here?
    DummyRenderOptions := TCastleRenderOptions.Create(nil);
    try
      SavedRenderMode := Renderer.RenderMode;
      Renderer.RenderMode := rmPrepareRenderSelf;

      DummyCamera := TRenderingCamera.Create;
      try
        DummyCamera.FromViewVectors(DefaultX3DCameraView, TMatrix4.Identity);

        Renderer.RenderBegin(nil, DummyCamera, nil, 0, 0, 0, @DummyStatistics);

        for I := 0 to Batching.PoolShapesCount - 1 do
        begin
          Shape := Batching.PoolShapes[I];
          TGLShape(Shape).Fog := nil;
          Renderer.RenderShape(TGLShape(Shape), DummyRenderOptions,
            TMatrix4.Identity, drFull);
        end;

        Renderer.RenderEnd;
      finally FreeAndNil(DummyCamera) end;

      Renderer.RenderMode := SavedRenderMode; // restore Renderer.RenderMode
    finally FreeAndNil(DummyRenderOptions) end;
  end;

begin
  if EffectiveDynamicBatching then
  begin
    BatchingShapesPrepareResources;
    BatchingShapesRender;
  end;
end;

procedure TShapesRenderer.GLContextClose;
begin
  if FOcclusionCullingRenderer <> nil then
    FOcclusionCullingRenderer.Utils.GLContextClose;

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

procedure TShapesRenderer.RenderShape_NoTests(
  const CollectedShape: TCollectedShape; const Params: TRenderParams);
var
  RenderOptions: TCastleRenderOptions; //< a shortcut for CollectedShape.RenderOptions
  Shape: TGLShape; //< a shortcut for CollectedShape.Shape

  procedure RenderNormal;
  var
    DepthRange: TDepthRange;
  begin
    { Shadow maps require normal DepthRange,
      don't mess with DepthRange in case when we render shadow maps. }
    if Params.RenderingCamera.Target in [rtShadowMap, rtVarianceShadowMap] then
      DepthRange := drFull
    else
      DepthRange := CollectedShape.DepthRange;

    Renderer.RenderShape(Shape,
      RenderOptions, CollectedShape.SceneTransform, DepthRange);
  end;

  procedure RenderWireframe(UseWireframeColor: boolean);
  var
    SavedMode: TRenderingMode;
    SavedSolidColor: TCastleColorRGB;
  begin
    {$ifndef OpenGLES} // TODO-es For OpenGLES, wireframe must be done differently
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    {$endif}

    if UseWireframeColor then
    begin
      SavedMode := RenderOptions.Mode;
      SavedSolidColor := RenderOptions.SolidColor;
      RenderOptions.Mode := rmSolidColor;
      RenderOptions.SolidColor := RenderOptions.WireframeColor;

      RenderNormal;

      RenderOptions.Mode := SavedMode;
      RenderOptions.SolidColor := SavedSolidColor;
    end else
    begin
      RenderNormal;
    end;

    { We restore by just assuming that default mode is GL_FILL.
      Nothing else in CGE changes glPolygonMode for now, so this is trivially true.

      This way we avoid using glPushAttrib / glPopAttrib to save state.
      They are

      1. deprecated,
      2. using them would break RenderContext state knowledge, causing problems later.

         Testcase:
         - in CGE editor,
         - activate shadow volumes on 1 light,
         - add 2nd light, not casting shadows (maybe not needed to reproduce),
         - make plane larger 100x100 (maybe not needed to reproduce),
         - add sphere and box,
         - add on them sphere and box collider,
         - activate "Physics -> Show Colliders".

         Using glPushAttrib / glPopAttrib would break rendering, making some
         objects weirdly wireframe depending on what was last hovered-over
         with a mouse in editor.  }

    {$ifndef OpenGLES} // TODO-es For OpenGLES, wireframe must be done differently
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    {$endif}
  end;

  { Render taking RenderOptions.WireframeEffect into account.
    Also controls Renderer.WireframePass,
    this way shaders for non-wireframe and wireframe can coexist,
    which avoids FPS drops e.g. at weSilhouette rendering a single 3D model
    (which would otherwise recreate all shaders in all frames). }
  procedure RenderWithWireframeEffect;
  var
    WireframeEffect: TWireframeEffect;
    SavedPolygonOffset: TPolygonOffset;
  begin
    WireframeEffect := RenderOptions.WireframeEffect;
    if InternalForceWireframe <> weNormal then
    begin
      { Do not allow InternalForceWireframe to fill (make non-wireframe) polygons
        that were supposed to be wireframe. This would look weird, e.g. some wireframe
        gizmos would become filled. }
      if not ( (WireframeEffect = weWireframeOnly) and
               (InternalForceWireframe = weSolidWireframe) ) then
        WireframeEffect := InternalForceWireframe;
    end;
    case WireframeEffect of
      weNormal:
        begin
          Renderer.WireframePass := 0;
          RenderNormal;
        end;
      weWireframeOnly:
        begin
          Renderer.WireframePass := 1;
          RenderWireframe(RenderOptions.Mode = rmSolidColor);
        end;
      weSolidWireframe:
        begin
          Renderer.WireframePass := 0;
          SavedPolygonOffset := RenderContext.PolygonOffset;
          RenderContext.PolygonOffsetEnable(RenderOptions.SolidWireframeScale, RenderOptions.SolidWireframeBias);
          RenderNormal;
          RenderContext.PolygonOffset := SavedPolygonOffset;

          Renderer.WireframePass := 1;
          RenderWireframe(true);
        end;
      weSilhouette:
        begin
          Renderer.WireframePass := 0;
          RenderNormal;

          Renderer.WireframePass := 1;
          SavedPolygonOffset := RenderContext.PolygonOffset;
          RenderContext.PolygonOffsetEnable(RenderOptions.SilhouetteScale, RenderOptions.SilhouetteBias);

          (* Old idea, may be resurrected one day:

          { rmSolidColor still does backface culling.
            This is very good in this case. When rmSolidColor and weSilhouette,
            and objects are solid (so backface culling is used) we can
            significantly improve the effect by reverting glFrontFace,
            this way we will cull *front* faces. This will not be noticed
            in case of rmSolidColor will single solid color, and it will
            improve the silhouette look, since front-face edges will not be
            rendered at all (no need to even hide them by glPolygonOffset,
            which is somewhat sloppy).

            TODO: this is probably incorrect now, that some meshes
            may have FrontFaceCcw = false.
            What we really would like to is to negate the FrontFaceCcw
            interpretation inside this RenderWireframe call.
          }
          if RenderOptions.Mode = rmSolidColor then
            glFrontFace(GL_CW);
          *)

          RenderWireframe(true);
          RenderContext.PolygonOffset := SavedPolygonOffset;
        end;
      else raise EInternalError.Create('Render: RenderOptions.WireframeEffect ?');
    end;
  end;

begin
  if Params.InternalPass = 0 then
  begin
    Inc(Params.Statistics.ShapesRendered);
    if Params.Transparent then
      Inc(Params.Statistics.ShapesRenderedBlending);
  end;

  Shape := CollectedShape.Shape;
  RenderOptions := CollectedShape.RenderOptions;

  // OcclusionBoxStateEnd will do nothing if OcclusionCulling = false
  FOcclusionCullingRenderer.Utils.OcclusionBoxStateEnd(false);

  FBlendingRenderer.BeforeRenderShape(Shape, RenderOptions);

  RenderWithWireframeEffect;
end;

procedure TShapesRenderer.RenderShape_OcclusionTests(
  const CollectedShape: TCollectedShape; const Params: TRenderParams);
begin
  { About "Params.RenderingCamera.Target = rtScreen" below:

    We do not make occlusion query when rendering to something else
    than screen (like shadow map or cube map environment for mirror).
    Such views are drastically different from normal camera view,
    so the whole idea that "what is visible in this frame is similar
    to what was visible in previous frame" breaks down there.

    TODO: In the future, this could be solved nicer, by having separate
    occlusion query states for different views. But this isn't easy
    to implement, as occlusion query state is part of TShape and
    octree nodes (for hierarchical occ query), so all these things
    should have a map "target->oq state" for various rendering targets. }

  if EffectiveOcclusionCulling and
      (Params.RenderingCamera.Target = rtScreen) then
  begin
    FOcclusionCullingRenderer.Render(CollectedShape, Params,
      {$ifdef FPC}@{$endif} RenderShape_NoTests);
  end else
    RenderShape_NoTests(CollectedShape, Params);
end;

procedure TShapesRenderer.Render(const Shapes: TShapesCollector;
  const Params: TRenderParams);

  procedure BatchingCommit;
  var
    Shape: TCollectedShape;
  begin
    if EffectiveDynamicBatching then
    begin
      Batching.Commit;
      for Shape in Batching.Batched do
      begin
        { Otherwise, shapes from batching FPool
          would never have PrepareResources called.
          Note: Unsure if this is still necessary. }
        Shape.Shape.PrepareResources;

        { Should we call RenderShape_NoTests or RenderShape_OcclusionTests?
          Doesn't matter, we know that occlusion culling is not done when
          batching is used, so RenderShape_OcclusionTests just calls
          RenderShape_NoTests. }
        RenderShape_NoTests(Shape, Params);
      end;
      Batching.FreeBatched;
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

  if EffectiveOcclusionCulling then
  begin
    FOcclusionCullingRenderer.Utils.ProjectionMatrix :=
      RenderContext.ProjectionMatrix;
    FOcclusionCullingRenderer.Utils.CameraMatrix :=
      Params.RenderingCamera.Matrix;
  end;

  { Initialize Batching.
    PreserveShapeOrder may be overridden to true below. }
  if EffectiveDynamicBatching then
  begin
    Batching.PreserveShapeOrder := false;
    Assert(Batching.Batched.Count = 0);
  end;

  if Params.Transparent then
  begin
    { We'll draw partially transparent objects now,
      only from scenes with RenderOptions.Blending. }

    if EffectiveDynamicBatching then
      Batching.PreserveShapeOrder := true;

    { TODO: The sorting is repeated at every render call.
      This is not optimal,
      we could instead reuse results from sorting in previous render call,
      if everything is still OK (nothing was added/removed from the world,
      order is still OK). }
    Shapes.FCollected.OnCustomShapeSort := OnCustomShapeSort;
    Shapes.FCollected.SortBackToFront(
      Params.RenderingCamera.View,
      BlendingSort);

    FBlendingRenderer.RenderBegin;
  end else
  begin
    Shapes.FCollected.OnCustomShapeSort := OnCustomShapeSort;
    Shapes.FCollected.SortFrontToBack(
      Params.RenderingCamera.View,
      OcclusionSort);
  end;

  Renderer.RenderBegin(Params.GlobalLights as TLightInstancesList,
    Params.RenderingCamera,
    LightRenderEvent, Params.InternalPass, { wireframe pass } 0, Params.UserPass,
    @Params.Statistics);
  try
    for CollectedShape in Shapes.FCollected do
    begin
      if not (EffectiveDynamicBatching and Batching.Batch(CollectedShape)) then
        RenderShape_OcclusionTests(CollectedShape, Params);
    end;

    BatchingCommit;

    { This must be called after BatchingCommit,
      since BatchingCommit may render some shapes }
    if Params.Transparent then
      FBlendingRenderer.RenderEnd;

    { As each RenderShape_OcclusionTests inside could set OcclusionBoxState,
      be sure to restore state now.
      OcclusionBoxStateEnd will do nothing if EffectiveOcclusionCulling = false. }
    FOcclusionCullingRenderer.Utils.OcclusionBoxStateEnd(true);
  finally Renderer.RenderEnd end;
end;

function TShapesRenderer.EffectiveDynamicBatching: boolean;
begin
  {$warnings off} // using deprecated CastleScene.DynamicBatching to keep it working
  Result :=
    (Self.DynamicBatching or CastleScene.DynamicBatching) and
    (not EffectiveOcclusionCulling);
  {$warnings on}
end;

function TShapesRenderer.EffectiveOcclusionCulling: Boolean;
begin
  Result :=
    OcclusionCulling and
    (GLFeatures <> nil) and // paranoid check from old code, maybe not needed anymore
    GLFeatures.OcclusionQuery and
    GLFeatures.VertexBufferObject and
    (GLFeatures.OcclusionQueryCounterBits > 0);
end;

procedure TShapesRenderer.SetOcclusionCulling(const Value: Boolean);
begin
  if FOcclusionCulling <> Value then
  begin
    FOcclusionCulling := Value;
  end;

  (*
  TODO:

  If OcclusionCulling just changed:
  If you switch OcclusionCulling on, then off, then move around the scene
  a lot, then switch OcclusionCulling back on --- you don't want to use
  results from previous query that was done many frames ago. }

  Old approach below. Should be done differently, looking at frame ids?

  procedure TCastleScene.ViewChangedSuddenly;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    inherited;

    if EffectiveOcclusionCulling then
    begin
      { Set OcclusionQueryAsked := false for all shapes. }
      ShapeList := Shapes.TraverseList(false, false, false);
      for Shape in ShapeList do
        TGLShape(Shape).OcclusionQueryAsked := false;
    end;
  end;
  *)

  { TODO: also Invalidate occlusion culling if camera moved a lot.

    Should take care of all viewpoints switching, like
    - switching to other viewpoint through view3dscene "viewpoints" menu,
    - just getting an event set_bind = true through vrml route.
    - calling Camera.SetView / SetWorldView to teleport.
  }
end;

end.
