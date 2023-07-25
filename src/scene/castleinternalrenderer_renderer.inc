{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Renderer class (TRenderer). }

{$ifdef read_interface}
//type
  TRenderer = class(TComponent)
  private
    { For speed, we keep a single instance of TShader,
      instead of creating / destroying an instance at each RenderShape.
      This is necessary, otherwise the constructor / destructor of TShader
      would be bottle-necks. }
    PreparedShader: TShader;

    { Custom shaders for shadow maps.
      Created on-demand, @nil initially. }
    VarianceShadowMapsShaders, ShadowMapsShaders: TCustomShaders;

    { ------------------------------------------------------------
      Things usable only during Render. }

    { How many texture units are used.

      It's always clamped by the number of available texture units
      (GLFeatures.MaxTextureUnits). Always <= 1 if OpenGL doesn't support
      multitexturing (not GLFeatures.UseMultiTexturing). }
    BoundTextureUnits: Cardinal;

    { For how many texture units do we have to generate tex coords.

      At the end, the idea is that this is <= BoundTextureUnits
      (no point in generating tex coords for not existing textures).
      However during render it may be temporarily > BoundTextureUnits
      (in case we calculate it before actually binding the textures,
      this may happen for textures in ComposedShader custom fields). }
    TexCoordsNeeded: Cardinal;

    { For which texture units we pushed and modified the texture matrix.
      Only inside RenderShape.
      Always <= 1 if not GLFeatures.UseMultiTexturing. }
    TextureTransformUnitsUsed: Cardinal;

    { Additional texture units used,
      in addition to 0..TextureTransformUnitsUsed - 1.
      Cleared by RenderShapeBegin, added by PushTextureUnit,
      used by RenderShapeEnd. }
    TextureTransformUnitsUsedMore: TInt32List;

    {$ifndef OpenGLES}
    { Call glPushMatrix, assuming that current matrix mode is GL_TEXTURE
      and current tex unit is TexUnit (always make sure this is true when
      calling it!).

      It also records this fact, so that RenderShapeEnd will be able to
      make pop texture matrix later.

      In fact this optimizes push/pops on texture matrix stack, such that
      X3D TextureTransform nodes and such together with PushTextureUnit
      will only use only matrix stack place, even if texture will be
      "pushed" multiple times (both by PushTextureUnit and normal
      X3D TextureTransform realized in RenderShapeBegin.) }
    procedure PushTextureUnit(const TexUnit: Cardinal);
    {$endif}

    { Check RenderOptions (like RenderOptions.BumpMapping) and OpenGL
      context capabilities to see if bump mapping can be used. }
    function BumpMapping(const RenderOptions: TCastleRenderOptions): TBumpMapping;

    {$I castleinternalrenderer_surfacetextures.inc}
  private
    { ----------------------------------------------------------------- }

    { Available between RenderBegin / RenderEnd. }
    LightsRenderer: TLightsRenderer;

    { Currently set fog parameters, during render. }
    FogFunctionality: TFogFunctionality;
    FogEnabled: boolean;
    FogType: TFogType;
    FogColor: TVector3;
    FogVisibilityRangeScaled: Single;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;

    { Lights shining on all shapes, may be @nil. Set in each RenderBegin. }
    GlobalLights: TLightInstancesList;

    { Rendering parameters. Set in each RenderBegin, cleared to nil in RenderEnd. }
    RenderingCamera: TRenderingCamera;
    Statistics: PRenderStatistics;

    { Rendering pass. Set in each RenderBegin and when changing WireframePass. }
    FPass: TTotalRenderingPass;

    FInternalPass: TInternalRenderingPass;
    FWireframePass: TWireframeRenderingPass;
    FUserPass: TUserRenderingPass;

    procedure SetWireframePass(const Value: TWireframeRenderingPass);

    { Calculate FPass based on FInternalPass, FWireframePass, FUserPass. }
    procedure UpdatePass;

    { Get X3D fog parameters, based on fog node and RenderOptions. }
    class procedure GetFog(const AFogFunctionality: TFogFunctionality;
      const RenderOptions: TCastleRenderOptions;
      out Enabled, Volumetric: boolean;
      out VolumetricDirection: TVector3;
      out VolumetricVisibilityStart: Single);

    { If multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (multitexturing availability, GL_TEXTURE0)
      is taken care of inside here. }
    class procedure ActiveTexture(const TextureUnit: Cardinal);

    { Disable any (fixed-function) texturing (2D, 3D, cube map, and so on)
      on given texture unit. }
    class procedure DisableTexture(const TextureUnit: Cardinal);
    class procedure DisableCurrentTexture;

    procedure RenderShapeLineProperties(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader);
    procedure RenderShapeMaterials(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader);
    procedure RenderShapeLights(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeFog(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeTextureTransform(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeClipPlanes(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeCreateMeshRenderer(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeShaders(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject);
    procedure RenderShapeTextures(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject;
      const UsedGLSLTexCoordsNeeded: Cardinal);
    procedure RenderShapeInside(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject);

    { Pieces of RenderBegin/End that are not dependent on any RenderBegin
      parameters, and actually could be used to include inside many sequences
      of RenderBegin/End. Like

        ClassRenderBegin
        RenderBegin
        RenderEnd
        RenderBegin
        RenderEnd
        RenderBegin
        RenderEnd
        ClassRenderEnd

      They are *not* used like this, though, now.
      They are just called from RenderBegin/End now.

      In the past we had ViewportRenderBegin/End, called by viewport,
      but then TRenderer state changes (e.g. depth range)
      could easily accidentally affect TCastleTransform custom rendering
      (e.g. using TRenderUnlitMesh). }
    class procedure ClassRenderBegin;
    class procedure ClassRenderEnd;
  public
    type
      TRenderMode = (
        { Normal rendering. }
        rmRender,
        { Prepare for future rendering of the same shapes. }
        rmPrepareRenderSelf,
        { Prepare for future rendering of the cloned shapes
          (made by TCastleSceneCore.Clone, or TX3DNode.DeepCopy). }
        rmPrepareRenderClones
      );
    var
      RenderMode: TRenderMode;

    { Constructor. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RenderBegin(const AGlobalLights: TLightInstancesList;
      const ARenderingCamera: TRenderingCamera;
      const LightRenderEvent: TLightRenderEvent;
      const AInternalPass: TInternalRenderingPass;
      const AWireframePass: TWireframeRenderingPass;
      const AUserPass: TUserRenderingPass;
      const AStatistics: PRenderStatistics);
    procedure RenderEnd;

    { Render given shape.

      @param(SceneTransform The transformation from scene to world,
        i.e. the transformation that should be multiplied by the Shape
        transformation (in @code(Shape.State.Transformation.Transform))
        to get the final transformation to world.)
    }
    procedure RenderShape(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions;
      const SceneTransform: TMatrix4;
      const DepthRange: TDepthRange);

    { Wireframe pass, to avoid recreating shaders when often switching
      between wireframe and non-wireframe rendering.
      This can change only between RenderBegin and RenderEnd.
      Each RenderBegin sets it (to given parameter). }
    property WireframePass: TWireframeRenderingPass
      read FWireframePass write SetWireframePass;
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TRenderer ---------------------------------------------------------- }

constructor TRenderer.Create(AOwner: TComponent);
begin
  inherited;
  TextureTransformUnitsUsedMore := TInt32List.Create;
  PreparedShader := TShader.Create;
end;

destructor TRenderer.Destroy;
begin
  FreeAndNil(TextureTransformUnitsUsedMore);
  FreeAndNil(PreparedShader);
  FreeAndNil(ShadowMapsShaders);
  FreeAndNil(VarianceShadowMapsShaders);
  inherited;
end;

function TRenderer.BumpMapping(const RenderOptions: TCastleRenderOptions): TBumpMapping;
begin
  if (RenderOptions.BumpMapping <> bmNone) and
     RenderOptions.Textures and
     (RenderOptions.Mode = rmFull) and
     GLFeatures.UseMultiTexturing and
     GLFeatures.Shaders then
    Result := RenderOptions.BumpMapping else
    Result := bmNone;
end;

class procedure TRenderer.ActiveTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.UseMultiTexturing then
    glActiveTexture(GL_TEXTURE0 + TextureUnit);
end;

class procedure TRenderer.DisableTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { This must be synchronized, and disable all that can be enabled
      by TShape.EnableTexture }
    ActiveTexture(TextureUnit);
    DisableCurrentTexture;
  end;
end;

class procedure TRenderer.DisableCurrentTexture;
begin
  GLEnableTexture(etNone);
end;

class procedure TRenderer.GetFog(const AFogFunctionality: TFogFunctionality;
  const RenderOptions: TCastleRenderOptions;
  out Enabled, Volumetric: boolean;
  out VolumetricDirection: TVector3;
  out VolumetricVisibilityStart: Single);
begin
  Enabled := (RenderOptions.Mode = rmFull) and
    (AFogFunctionality <> nil) and
    (AFogFunctionality.VisibilityRange <> 0.0);
  Volumetric := Enabled and
    AFogFunctionality.Volumetric and
    // Volumetric fog is now only supported by shaders
    (not GLFeatures.EnableFixedFunction);

  if Volumetric then
  begin
    VolumetricVisibilityStart :=
      AFogFunctionality.VolumetricVisibilityStart *
      AFogFunctionality.TransformScale;
    VolumetricDirection := AFogFunctionality.VolumetricDirection;
  end else
  begin
    { whatever, just set them to any determined values }
    VolumetricVisibilityStart := 0;
    VolumetricDirection := TVector3.Zero;
  end;
end;

class procedure TRenderer.ClassRenderBegin;
{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    { ------------------------------------------------------------------------

      Set fixed-function state that can be assumed by TRenderer,
      and may change during rendering but will always be restored later.
      So this secures from other rendering code (outside of TRenderer)
      setting this OpenGL state to something unexpected.

      E.g. shape may assume that GL_COLOR_MATERIAL is disabled.
      If a shape does glEnable(GL_COLOR_MATERIAL), then it will for sure also do glDisable(GL_COLOR_MATERIAL)
      at the end, so that state after shape rendering stays as it was.

      This generally sets "typical" OpenGL state, and can be saved/restored during TCastleScene rendering.
    }

    // set texture unit to 0, this determines what is affected by glDisable(GL_TEXTURE_GEN_xxx)
    if GLFeatures.UseMultiTexturing then
    begin
      ActiveTexture(0);
      glClientActiveTexture(GL_TEXTURE0);
    end;
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);

    glDisable(GL_COLOR_MATERIAL);

    for I := 0 to GLFeatures.MaxTextureUnits - 1 do
      DisableTexture(I);

    { ------------------------------------------------------------------------

      Set fixed-function state that can be assumed by TRenderer,
      and is constant for all shapes and for all scenes
      (so it doesn't even depend on RenderOptions),
      and we don't care that we just changed it also
      for stuff outside of TRenderer (so we make no attempt to restore
      it at ClassRenderEnd or even track previous state).

      This generally sets "typical" OpenGL state, and remains constant during
      RenderBegin/End rendering. }

    { About using color material (what does glColor mean for materials):
      - We always set diffuse material component from the color.
        This satisfies all cases.
      - TColorPerVertexCoordinateRenderer.RenderCoordinateBegin
        takes care of actually enabling COLOR_MATERIAL, it assumes that
        the state is as set below.
      - We never change glColorMaterial during rendering,
        so no need to call this in RenderEnd. }
    glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);

    glShadeModel(GL_SMOOTH);

    { While we don't *really need* to enable GL_NORMALIZE,
      as we always try to provide normalized normals,
      but avoiding GL_NORMALIZE doesn't give us *any* performance benefits.

      And enabling GL_NORMALIZE:
      - make us correct even when glTF/X3D files provided unnormalized normals.
      - in case the object is scaled.
      - it is also consistent with what shader pipeline is doing, it is always
        normalizing normals. }
    glEnable(GL_NORMALIZE);

    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  end;
  {$endif}
end;

class procedure TRenderer.ClassRenderEnd;
{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  { Restore defaults, for other rendering code outside of TRenderer.

    These RenderContext properties are always set before rendering every shape,
    so ClassRenderBegin doesn't care to set them to any value,
    and we just reset them in ClassRenderEnd. }
  RenderContext.CullFace := false;
  RenderContext.FrontFaceCcw := true;
  RenderContext.CurrentProgram := nil;
  RenderContext.FixedFunctionAlphaTestDisable;
  RenderContext.FixedFunctionLighting := false;
  RenderContext.DepthRange := DefaultDepthRange;

  { Restore defaults, for other rendering code outside of TRenderer.

    These RenderContext properties are always set in InitializeFromRenderOptions,
    so they are constant for each shape.
    So ClassRenderBegin doesn't care to set them to any value,
    and we just reset them in ClassRenderEnd. }
  RenderContext.PointSize := 1;
  RenderContext.LineWidth := 1;
  RenderContext.DepthTest := false;
  RenderContext.ColorChannels := AllColorChannels;

  { Restore default fixed-function state, for other rendering code outside of TRenderer.

    These state things are adjusted by each shape,
    and shape rendering assumes that at the beginning it is undefined
    (so we don't care to set it in ClassRenderBegin),
    and shape doesn't restore it (because next shape will adjust it anyway).
    So we reset them at the end to not affect other objects rendered outside of TRenderer.

    This generally sets "typical" OpenGL state,
    for things that may have non-standard values during TCastleScene rendering,
    and each shape is prepared that it has non-standard value (but rendering outside
    of TRenderer may not be prepared for it). }

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    for I := 0 to GLFeatures.MaxLights - 1 do
      glDisable(GL_LIGHT0 + I);
    glDisable(GL_FOG);

    { Restore active texture unit to 0.
      This also sets texture unit for subsequent glTexEnvi call. }
    if GLFeatures.UseMultiTexturing then
    begin
      ActiveTexture(0);
      {$ifndef OpenGLES}
      glClientActiveTexture(GL_TEXTURE0);
      {$endif}
    end;

    { Reset GL_TEXTURE_ENV, otherwise it may be left GL_COMBINE
      after rendering X3D model using MultiTexture. }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  end;
  {$endif}
end;

procedure TRenderer.RenderBegin(
  const AGlobalLights: TLightInstancesList;
  const ARenderingCamera: TRenderingCamera;
  const LightRenderEvent: TLightRenderEvent;
  const AInternalPass: TInternalRenderingPass;
  const AWireframePass: TWireframeRenderingPass;
  const AUserPass: TUserRenderingPass;
  const AStatistics: PRenderStatistics);
begin
  ClassRenderBegin;

  GlobalLights := AGlobalLights;
  RenderingCamera := ARenderingCamera;
  Statistics := AStatistics;
  Assert(RenderingCamera <> nil);

  Assert(FogFunctionality = nil);
  Assert(not FogEnabled);

  LightsRenderer := TLightsRenderer.Create;
  LightsRenderer.LightRenderEvent := LightRenderEvent;
  LightsRenderer.RenderingCamera := RenderingCamera;

  FInternalPass := AInternalPass;
  FWireframePass := AWireframePass;
  FUserPass := AUserPass;
  UpdatePass;
end;

procedure TRenderer.RenderEnd;
begin
  { Tests:
  Writeln('LightsRenderer stats: light setups done ',
    LightsRenderer.Statistics[true], ' vs avoided ',
    LightsRenderer.Statistics[false]); }

  FreeAndNil(LightsRenderer);

  FogFunctionality := nil;
  FogEnabled := false;

  { We need this in RenderEnd, not only in ClassRenderEnd.
    Otherwise some operations could make the current texture unit not bound
    to any valid texture, causing the glUniform1i setting shader uniform for texture
    to raise OpenGL errors.
    Testcase: 3d_model_viewer template. }
  RenderContext.CurrentProgram := nil;

  RenderingCamera := nil;
  Statistics := nil;

  ClassRenderEnd;
end;

procedure TRenderer.RenderShape(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const SceneTransform: TMatrix4;
  const DepthRange: TDepthRange);

  procedure InitializeFromRenderOptions;
  begin
    { Set RenderContext properties, determined by RenderOptions.

      These do not change within one TCastleScene,
      but since the shape rendering order is now independent of TCastleScene
      division (TShapesRenderer may render shapes from different TCastleScenes
      in any order), so we have to assume that each RenderShape changes
      RenderOptions.

      Note that all TCastleTransform rendering code that is not inside TCastleScene
      (e.g. custom TCastleTransform descendant using TCastleRenderUnlitMesh)
      must be prepared that this state may be whatever, and so it must save,
      set and restore this state.

      ClassRenderEnd resets these things, so that this state does not "leak"
      for stuff rendered outside of TCastleViewport. }
    RenderContext.PointSize := RenderOptions.PointSize;
    RenderContext.LineWidth := RenderOptions.LineWidth;
    RenderContext.DepthTest := RenderOptions.DepthTest;
    RenderContext.ColorChannels := RenderOptions.InternalColorChannels;

    LightsRenderer.MaxLightsPerShape := RenderOptions.MaxLightsPerShape;
  end;

  { Initialize new Shader instance.
    (May reuse previous Shader instance, but then it clears its contents.) }
  function InitializeShader: TShader;

    function ShapeUsesEnvironmentLight(const Shape: TX3DRendererShape): boolean;
    var
      I: Integer;
      Lights: TLightInstancesList;
    begin
      Lights := Shape.State.Lights;
      if Lights <> nil then
        for I := 0 to Lights.Count - 1 do
          if Lights.L[I].Node is TEnvironmentLightNode then
            Exit(true);
      Result := false;
    end;

    function ShapeMaybeUsesShadowMaps(const Shape: TX3DRendererShape): boolean;
    var
      Tex, SubTexture: TX3DNode;
      I: Integer;
    begin
      Result := false;
      if (Shape.Node <> nil) and
        (Shape.Node.Appearance <> nil) then
      begin
        Tex := Shape.Node.Appearance.Texture;

        if Tex is TGeneratedShadowMapNode then
          Exit(true);

        if Tex is TMultiTextureNode then
        begin
          for I := 0 to TMultiTextureNode(Tex).FdTexture.Count - 1 do
          begin
            SubTexture := TMultiTextureNode(Tex).FdTexture[I];
            if SubTexture is TGeneratedShadowMapNode then
              Exit(true);
          end;
        end;
      end;
    end;

    function ShapeMaterialRequiresPhongShading(const Shape: TX3DRendererShape): boolean;
    begin
      Result :=
        (Shape.Node <> nil) and
        (Shape.Node.Appearance <> nil) and
        (Shape.Node.Appearance.Material is TPhysicalMaterialNode);
    end;

  var
    PhongShading: boolean;
  begin
    { instead of TShader.Create, reuse existing PreparedShader for speed }
    Result := PreparedShader;
    Result.Clear;
    Result.RenderingCamera := RenderingCamera;

    { calculate PhongShading }
    PhongShading := RenderOptions.PhongShading;
    { if Shape specifies Shading = Gouraud or Phong, use it }
    if Shape.Node <> nil then
      if Shape.Node.Shading = shPhong then
        PhongShading := true
      else
      if Shape.Node.Shading = shGouraud then
        PhongShading := false;
    { if some feature requires PhongShading, make it true }
    if ShapeMaybeUsesSurfaceTexture(Shape) or
      ShapeMaybeUsesShadowMaps(Shape) or
      ShapeMaterialRequiresPhongShading(Shape) or
      ShapeUsesEnvironmentLight(Shape) or
      (not Shape.Geometry.Solid) { two-sided lighting required by solid=FALSE } then
      PhongShading := true;

    Result.Initialize(PhongShading);

    if PhongShading then
      Result.ShapeRequiresShaders := true;

    Result.ShapeBoundingBoxInSceneEvent := {$ifdef FPC}@{$endif} Shape.BoundingBox;
    Result.SceneTransform := SceneTransform;
    Result.SceneModelView := RenderingCamera.CurrentMatrix * SceneTransform;
    Result.ShadowSampling := RenderOptions.ShadowSampling;
  end;

  procedure FixedFunctionBegin(const SceneModelView: TMatrix4);
  begin
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      glPushMatrix;
      glLoadMatrix(SceneModelView);

      if RenderOptions.Mode = rmSolidColor then
        glColorv(RenderOptions.SolidColor);
    end;
    {$endif}
  end;

  procedure FixedFunctionEnd;
  begin
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
      glPopMatrix;
    {$endif}
  end;

var
  Shader: TShader;
begin
  InitializeFromRenderOptions;

  RenderContext.DepthRange := DepthRange;

  Shader := InitializeShader;
  FixedFunctionBegin(Shader.SceneModelView);

  RenderShapeLineProperties(Shape, RenderOptions, Shader);

  FixedFunctionEnd;
end;

procedure TRenderer.RenderShapeLineProperties(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader);
var
  LP: TLinePropertiesNode;
begin
  if (Shape.Node <> nil) and { Shape.Node is nil for VRML <= 1.0 }
     (Shape.Node.Appearance <> nil) then
    LP := Shape.Node.Appearance.LineProperties
  else
    LP := nil;

  if (LP <> nil) and LP.FdApplied.Value then
  begin
    RenderContext.LineWidth := Max(1.0, RenderOptions.LineWidth * LP.FdLineWidthScaleFactor.Value);
    RenderContext.LineType := LP.LineType;
  end else
  begin
    RenderContext.LineWidth := RenderOptions.LineWidth;
    RenderContext.LineType := ltSolid;
  end;

  RenderShapeMaterials(Shape, RenderOptions, Shader);
end;

procedure TRenderer.RenderShapeMaterials(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader);

  {$I castleinternalrenderer_materials.inc}

begin
  RenderMaterialsBegin;
  RenderShapeLights(Shape, RenderOptions, Shader, Lighting);
end;

procedure TRenderer.RenderShapeLights(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean);
var
  FinalGlobalLights, SceneLights: TLightInstancesList;
begin
  { This is done after setting Shader.MaterialSpecularColor
    by RenderMaterialsBegin,
    as MaterialSpecularColor must be already set during Shader.EnableLight. }

  { When lighting is off (for either shaders or fixed-function),
    there is no point in setting up lights. }
  if Lighting then
  begin
    if RenderOptions.ReceiveGlobalLights then
      FinalGlobalLights := GlobalLights
    else
      FinalGlobalLights := nil;

    if RenderOptions.ReceiveSceneLights then
      SceneLights := Shape.State.Lights
    else
      SceneLights := nil;

    LightsRenderer.Render(Shape, FinalGlobalLights, SceneLights, Shader);
  end;

  RenderShapeFog(Shape, RenderOptions, Shader, Lighting);
end;

procedure TRenderer.RenderShapeFog(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean);

const
  FogCoordinateSource: array [boolean { volumetric }] of TFogCoordinateSource =
  ( fcDepth, fcPassedCoordinate );

  { Set OpenGL fog based on given fog node. Returns also fog parameters,
    like GetFog. }
  procedure RenderFog(const AFogFunctionality: TFogFunctionality;
    out Volumetric: boolean;
    out VolumetricDirection: TVector3;
    out VolumetricVisibilityStart: Single);
  var
    VisibilityRangeScaled: Single;
  begin
    GetFog(AFogFunctionality, RenderOptions,
      FogEnabled, Volumetric, VolumetricDirection, VolumetricVisibilityStart);

    if FogEnabled then
    begin
      Assert(AFogFunctionality <> nil);

      VisibilityRangeScaled :=
        AFogFunctionality.VisibilityRange *
        AFogFunctionality.TransformScale;

      { calculate FogType and other Fog parameters }
      FogType := AFogFunctionality.FogType;
      FogColor := AFogFunctionality.Color;
      FogVisibilityRangeScaled := VisibilityRangeScaled;
    end;
  end;

const
  FogConstantDensityFactor = 3.0; //< Necessary for exponential fog in fixed-function
begin
  { Enable / disable fog and set fog parameters if needed }
  if FogFunctionality <> Shape.Fog then
  begin
    FogFunctionality := Shape.Fog;
    RenderFog(FogFunctionality, FogVolumetric,
      FogVolumetricDirection, FogVolumetricVisibilityStart);

    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      { Set fixed-function fog parameters. }
      if FogEnabled then
      begin
        glFogv(GL_FOG_COLOR, Vector4(FogColor, 1.0));
        case FogType of
          ftLinear:
            begin
              glFogi(GL_FOG_MODE, GL_LINEAR);
              glFogf(GL_FOG_START, 0);
              glFogf(GL_FOG_END, FogVisibilityRangeScaled);
            end;
          ftExponential:
            begin
              glFogi(GL_FOG_MODE, GL_EXP);
              glFogf(GL_FOG_DENSITY, FogConstantDensityFactor / FogVisibilityRangeScaled);
            end;
          {$ifndef COMPILER_CASE_ANALYSIS}
          else raise EInternalError.Create('TRenderer.RenderShapeFog:FogType? 2');
          {$endif}
        end;
        { We want to be able to render any scene --- so we have to be prepared
          that fog interpolation has to be corrected for perspective. }
        glHint(GL_FOG_HINT, GL_NICEST);
        glEnable(GL_FOG);
      end else
        glDisable(GL_FOG);
    end;
    {$endif}
  end;

  if FogEnabled then
    Shader.EnableFog(FogType, FogCoordinateSource[FogVolumetric],
      FogColor, FogVisibilityRangeScaled);
  RenderShapeTextureTransform(Shape, RenderOptions, Shader, Lighting);
end;

procedure TRenderer.RenderShapeTextureTransform(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader; const Lighting: boolean);
var
  TextureTransform: TAbstractTextureTransformNode;
  Child: TX3DNode;
  Transforms: TMFNode;
  I, FirstTexUnit: Integer;
  State: TX3DGraphTraverseState;
  Matrix: TMatrix4;
begin
  TextureTransformUnitsUsed := 0;
  TextureTransformUnitsUsedMore.Count := 0;

  { in case of FontTextureNode <> nil, 1 less texture unit is available.
    Everything we do, must be shifted by 1 texture unit. }
  if Shape.OriginalGeometry.FontTextureNode <> nil then
    FirstTexUnit := 1 else
    FirstTexUnit := 0;

  State := Shape.State;

  if (State.ShapeNode = nil { VRML 1.0, always some texture transform }) or
     ( (State.ShapeNode <> nil) and
       (State.ShapeNode.Appearance <> nil) and
       (State.ShapeNode.Appearance.TextureTransform <> nil { VRML 2.0 with tex transform })
     ) then
  begin
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      glMatrixMode(GL_TEXTURE);
    end;
    {$endif}

    { We work assuming that texture matrix before RenderShape was identity.
      Texture transform encoded in VRML/X3D will be multiplied by this.

      This allows the programmer to eventually transform all textures
      by placing non-identity texture matrix (just like a programmer
      can transform whole rendered model by changing modelview matrix).
      So this is a good thing.

      Additional advantage is that we do not have to explicitly "clear"
      the texture matrix if it's an identity transformation in VRML/X3D.
      We just let it stay like it was.

      This also nicely cooperates with X3D MultiTextureTransform desired
      behavior: "If there are too few entries in the textureTransform field,
      identity matrices shall be used for all remaining undefined channels.".
      Which means that looking at MultiTextureTransform node, we know exactly
      on which texture units we have to apply transform: we can leave
      the remaining texture units as they were, regardless of whether
      MultiTexture is used at all and regardless of how many texture units
      are actually used by MultiTexture. }

    { TODO: for bump mapping, TextureTransform should be done on more than one texture unit. }

    if State.ShapeNode = nil then
    begin
      { No multitexturing in VRML 1.0, just always transform first tex unit. }
      if FirstTexUnit < GLFeatures.MaxTextureUnits then
      begin
        TextureTransformUnitsUsed := 1;
        if GLFeatures.EnableFixedFunction then
        begin
          {$ifndef OpenGLES}
          ActiveTexture(FirstTexUnit);
          glPushMatrix;
          glMultMatrix(State.TextureTransform);
          {$endif}
        end;
        Shader.EnableTextureTransform(FirstTexUnit, State.TextureTransform);
      end;
    end else
    begin
      // conditions above should have already checked this, we have some texture transform
      Assert(State.ShapeNode <> nil);
      Assert(State.ShapeNode.Appearance <> nil);
      Assert(State.ShapeNode.Appearance.TextureTransform <> nil);
      TextureTransform := State.ShapeNode.Appearance.TextureTransform;
      if TextureTransform <> nil then
      begin
        if TextureTransform is TMultiTextureTransformNode then
        begin
          Transforms := TMultiTextureTransformNode(TextureTransform).FdTextureTransform;

          { Multitexturing, so use as many texture units as there are children in
            MultiTextureTransform.textureTransform.
            Cap by available texture units. }
          TextureTransformUnitsUsed := Min(Transforms.Count,
            GLFeatures.MaxTextureUnits - FirstTexUnit);

          for I := 0 to TextureTransformUnitsUsed - 1 do
          begin
            if GLFeatures.EnableFixedFunction then
            begin
              {$ifndef OpenGLES}
              ActiveTexture(FirstTexUnit + I);
              glPushMatrix;
              {$endif}
            end;
            Child := Transforms[I];
            if (Child <> nil) and
               (Child is TAbstractTextureTransformNode) then
            begin
              if Child is TMultiTextureTransformNode then
                WritelnWarning('VRML/X3D', 'MultiTextureTransform.textureTransform list cannot contain another MultiTextureTransform instance') else
              begin
                Matrix := TAbstractTextureTransformNode(Child).TransformMatrix;
                if GLFeatures.EnableFixedFunction then
                begin
                  {$ifndef OpenGLES}
                  glMultMatrix(Matrix);
                  {$endif}
                end;
                Shader.EnableTextureTransform(FirstTexUnit + I, Matrix);
              end;
            end;
          end;
        end else
        { Check below is done because X3D specification explicitly
          says that MultiTexture is affected *only* by MultiTextureTransform,
          that is normal TextureTransform and such is ignored (treated
          like identity transform, *not* applied to 1st texture unit).

          By the way, we don't do any texture transform if Texture = nil,
          since then no texture is used anyway.

          TODO: what to do with CommonSurfaceShader ?

          TODO: fix for new texture channels, where one TextureTransform
          should work like MultiTextureTransform with one item. }
        if (State.MainTexture <> nil) and
           (not (State.MainTexture is TMultiTextureNode)) then
        begin
          if FirstTexUnit < GLFeatures.MaxTextureUnits then
          begin
            TextureTransformUnitsUsed := 1;
            Matrix := TextureTransform.TransformMatrix;
            if GLFeatures.EnableFixedFunction then
            begin
              {$ifndef OpenGLES}
              ActiveTexture(FirstTexUnit);
              glPushMatrix;
              glMultMatrix(Matrix);
              {$endif}
            end;
            Shader.EnableTextureTransform(FirstTexUnit, Matrix);
          end;
        end;
      end;
    end;

    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      { restore GL_MODELVIEW }
      glMatrixMode(GL_MODELVIEW);
    end;
    {$endif}
  end;

  RenderShapeClipPlanes(Shape, RenderOptions, Shader, Lighting);

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    if (TextureTransformUnitsUsed <> 0) or
       (TextureTransformUnitsUsedMore.Count <> 0) then
    begin
      glMatrixMode(GL_TEXTURE);

      for I := 0 to TextureTransformUnitsUsed - 1 do
      begin
        { This code is Ok also when not GLFeatures.UseMultiTexturing: then
          TextureTransformUnitsUsed for sure is <= 1 and ActiveTexture
          will be simply ignored. }
        ActiveTexture(FirstTexUnit + I);
        glPopMatrix;
      end;

      for I := 0 to TextureTransformUnitsUsedMore.Count - 1 do
      begin
        ActiveTexture(TextureTransformUnitsUsedMore.L[I]);
        glPopMatrix;
      end;

      { restore GL_MODELVIEW }
      glMatrixMode(GL_MODELVIEW);
    end;
    {$endif}
  end;
end;

procedure TRenderer.RenderShapeClipPlanes(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean);
var
  { How many clip planes were enabled (and so, how many must be disabled
    at the end). }
  ClipPlanesEnabled: Cardinal;

  { Initialize OpenGL clip planes, looking at ClipPlanes list.
    We know we're inside GL_MODELVIEW mode,
    and we know all clip planes are currently disabled. }
  procedure ClipPlanesBegin(ClipPlanes: TClipPlaneList);
  var
    I: Integer;
    ClipPlane: PClipPlane;
  begin
    ClipPlanesEnabled := 0;
    { GLMaxClipPlanes should be >= 6 with every conforming OpenGL,
      but still better check. }
    if (GLFeatures.MaxClipPlanes > 0) and (ClipPlanes <> nil) then
      for I := 0 to ClipPlanes.Count - 1 do
      begin
        ClipPlane := PClipPlane(ClipPlanes.Ptr(I));
        if ClipPlane^.Node.FdEnabled.Value then
        begin
          Assert(ClipPlanesEnabled < GLFeatures.MaxClipPlanes);
          Shader.EnableClipPlane(ClipPlanesEnabled,
            PlaneTransform(ClipPlane^.Node.FdPlane.Value, ClipPlane^.Transform));
          Inc(ClipPlanesEnabled);

          { No more clip planes possible, regardless if there are any more
            enabled clip planes on the list. }
          if ClipPlanesEnabled = GLFeatures.MaxClipPlanes then Break;
        end;
      end;
  end;

  { Disable OpenGL clip planes previously initialized by ClipPlanesBegin. }
  procedure ClipPlanesEnd;
  var
    I: Integer;
  begin
    for I := 0 to Integer(ClipPlanesEnabled) - 1 do
      Shader.DisableClipPlane(I);
    ClipPlanesEnabled := 0; { not really needed, but for safety... }
  end;

begin
  { This must be done before "glMultMatrix(Shape.State.Transform)" below,
    as in case of fixed-function pipeline the ClipPlanesBegin
    causes glClipPlane that sets clip plane assuming the current matrix
    contains only camera. }
  ClipPlanesBegin(Shape.State.ClipPlanes);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;
      glMultMatrix(Shape.State.Transformation.Transform);
  end;
  {$endif}

    RenderShapeCreateMeshRenderer(Shape, RenderOptions, Shader, Lighting);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
    glPopMatrix;
  {$endif}

  ClipPlanesEnd;
end;

procedure TRenderer.RenderShapeCreateMeshRenderer(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader; const Lighting: boolean);
var
  GeneratorClass: TArraysGeneratorClass;
  MeshRenderer: TMeshRenderer;

  { If Shape.Geometry should be rendered using one of TMeshRenderer
    classes, then create appropriate MeshRenderer and return @true.
    Otherwise return @false and doesn't set MeshRenderer.

    Takes care of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render. }
  function InitMeshRenderer: boolean;
  begin
    GeneratorClass := GetArraysGenerator(Shape.Geometry);
    Result := GeneratorClass <> nil;
    if Result then
    begin
      { If we have GeneratorClass, create TCompleteCoordinateRenderer.
        We'll initialize TCompleteCoordinateRenderer.Arrays later. }
      MeshRenderer := TCompleteCoordinateRenderer.Create(Self, Shape);
      MeshRenderer.ShapeModelView :=
        Shader.SceneModelView * Shape.State.Transformation.Transform;
      MeshRenderer.RenderOptions := RenderOptions;
      Shape.BumpMappingAllowed := GeneratorClass.BumpMappingAllowed;
    end;
  end;

begin
  { default Shape.BumpMapping* state }
  Shape.BumpMappingAllowed := false;
  Shape.BumpMappingUsed := false;

  { Initalize MeshRenderer to something non-nil. }
  if not InitMeshRenderer then
  begin
    WritelnWarning('VRML/X3D', Format('Rendering of node kind "%s" not implemented',
      [Shape.NiceName]));
    Exit;
  end;

  Assert(MeshRenderer <> nil);

  try
    RenderShapeShaders(Shape, RenderOptions, Shader, Lighting,
      GeneratorClass, MeshRenderer);
  finally
    FreeAndNil(MeshRenderer);
  end;
end;

procedure TRenderer.RenderShapeShaders(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject);
var
  { > 0 means that we had custom shader node *and* it already
    needs given number texture units. Always 0 otherwise. }
  UsedGLSLTexCoordsNeeded: Cardinal;

  function TextureCoordsDefined: Cardinal;
  var
    TexCoord: TX3DNode;
  begin
    if Shape.Geometry.InternalTexCoord(Shape.State, TexCoord) and
       (TexCoord <> nil) then
    begin
      if TexCoord is TMultiTextureCoordinateNode then
        Result := TMultiTextureCoordinateNode(TexCoord).FdTexCoord.Count else
        Result := 1;
    end else
      Result := 0;
  end;

  function TextureUnitsDefined(Node: TComposedShaderNode): Cardinal;

    function TextureUnits(Node: TX3DNode): Cardinal;
    begin
      if Node is TMultiTextureNode then
        Result := TMultiTextureNode(Node).FdTexture.Count else
      if Node is TAbstractTextureNode then
        Result := 1 else
        Result := 0;
    end;

  var
    I, J: Integer;
    UniformField: TX3DField;
    IDecls: TX3DInterfaceDeclarationList;
  begin
    IDecls := Node.InterfaceDeclarations;
    Result := 0;
    Assert(IDecls <> nil);
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
          Result := Result + TextureUnits(TSFNode(UniformField).Value) else
        if UniformField is TMFNode then
          for J := 0 to TMFNode(UniformField).Count - 1 do
            Result := Result + TextureUnits(TMFNode(UniformField)[J]);
      end;
    end;

    if Shape.OriginalGeometry.FontTextureNode <> nil then
      Inc(Result);
  end;

var
  TCD: Cardinal;
  UsedShaderNode: TComposedShaderNode;
begin
  { Use custom shader code (ComposedShader) if available. }

  UsedGLSLTexCoordsNeeded := 0;

  if (Shape.Node <> nil) and
     (Shape.Node.Appearance <> nil) and
     Shader.EnableCustomShaderCode(Shape.Node.Appearance.FdShaders, UsedShaderNode) then
  begin
    UsedGLSLTexCoordsNeeded := TextureUnitsDefined(UsedShaderNode);

    { Only if we bound texture units defined in shader ComposedShader fields
      (it we have shader but UsedGLSLTexCoordsNeeded = 0 then normal
      texture apply (including normal TexCoordsNeeded calculation)
      will be done):

      Although we bound only UsedGLSLTexCoordsNeeded texture units,
      we want to pass all texture coords defined in texCoord.
      Shaders may use them (even when textures are not bound for them). }

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      TCD := TextureCoordsDefined;
      if TCD > UsedGLSLTexCoordsNeeded then
        WritelnLog('TexCoord', Format('Texture coords defined in VRML/X3D for %d texture units, using them all, even though we bound only %d texture units. Reason: GLSL shaders may use them',
          [TCD, UsedGLSLTexCoordsNeeded]));
      MaxVar(UsedGLSLTexCoordsNeeded, TCD);
    end;
  end;

  RenderShapeTextures(Shape, RenderOptions, Shader, Lighting,
    GeneratorClass, TMeshRenderer(ExposedMeshRenderer), UsedGLSLTexCoordsNeeded);
end;

procedure TRenderer.RenderShapeTextures(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject;
  const UsedGLSLTexCoordsNeeded: Cardinal);

  function NodeTextured(Node: TAbstractGeometryNode): boolean;
  begin
    Result := not (
      (Node is TPointSetNode) or
      (Node is TIndexedLineSetNode));
  end;

  procedure EnableOneShadowMap(const Texture: TGeneratedShadowMapNode;
    var TexCoordsNeeded: Cardinal; const Shader: TShader);
  var
    GLTexture: TTextureResource;
  begin
    GLTexture := TTextureResources.Get(Texture);
    if GLTexture <> nil then
      GLTexture.EnableAll(Self, GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
  end;

  procedure EnableShadowMaps(const Texture: TAbstractTextureNode;
    var TexCoordsNeeded: Cardinal; const Shader: TShader);
  var
    I: Integer;
    ChildNode: TX3DNode;
  begin
    if Texture is TGeneratedShadowMapNode then
    begin
      EnableOneShadowMap(TGeneratedShadowMapNode(Texture), TexCoordsNeeded, Shader);
    end else
    if Texture is TMultiTextureNode then
      for I := 0 to TMultiTextureNode(Texture).FdTexture.Count - 1 do
      begin
        { TODO: This is not really correct, the texture unit number
          (TexCoordsNeeded) may not correspond to the texture coordinate number
          where the ProjectedTextureCoordinate node is. }

        ChildNode := TMultiTextureNode(Texture).FdTexture[I];
        if ChildNode is TGeneratedShadowMapNode then
          EnableOneShadowMap(TGeneratedShadowMapNode(ChildNode), TexCoordsNeeded, Shader);
      end;
  end;

  function GetAlphaCutoff(const Shape: TShape): Single;
  begin
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
      Result := Shape.Node.Appearance.AlphaCutoff
    else
      Result := DefaultAlphaCutoff;
  end;

  procedure RenderTexturesBegin;
  var
    TextureNode: TAbstractTextureNode;
    TextureRes: TTextureResource;
    AlphaTest: Boolean;
    AlphaCutoff: Single;
    FontTextureNode: TAbstractTexture2DNode;
    FontTextureRes: TTextureResource;
    MainTextureMapping: Integer;
  begin
    TexCoordsNeeded := 0;
    BoundTextureUnits := 0;

    if RenderOptions.Mode = rmSolidColor then
    begin
      { Make sure each shape sets fixed-function alpha test,
        regardless of RenderOptions.Mode (code for other RenderOptions.Mode values
        is lower in this routine), otherwise it is undefined. }
      RenderContext.FixedFunctionAlphaTestDisable;
      Exit;
    end;

    AlphaTest := false;

    TextureNode := Shape.State.MainTexture(Shape.Geometry, MainTextureMapping);
    TextureRes := TTextureResources.Get(TextureNode);
    { assert we never have non-nil TextureRes and nil TextureNode }
    Assert((TextureRes = nil) or (TextureNode <> nil));

    Shader.MainTextureMapping := MainTextureMapping;

    FontTextureNode := Shape.OriginalGeometry.FontTextureNode;
    FontTextureRes := TTextureResources.Get(FontTextureNode);
    { assert we never have non-nil FontTextureRes and nil FontTextureNode }
    Assert((FontTextureRes = nil) or (FontTextureNode <> nil));

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      { Do not bind/enable normal textures. Just set TexCoordsNeeded
        to generate tex coords for textures used in the shader.
        Leave BoundTextureUnits at 0 (BoundTextureUnits will be increased
        later when shader actually binds texture uniform values). }
      TexCoordsNeeded := UsedGLSLTexCoordsNeeded;
    end else
    if RenderOptions.Textures and
       NodeTextured(Shape.Geometry) then
    begin
      AlphaTest := TGLShape(Shape).AlphaChannel = acTest;

      if FontTextureRes <> nil then
        FontTextureRes.EnableAll(Self, GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
      if TextureRes <> nil then
        TextureRes.EnableAll(Self, GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
      BoundTextureUnits := TexCoordsNeeded;

      if (Shape.Node <> nil) and
         (Shape.Node.Appearance <> nil) and
         (Shape.Node.Appearance.Texture <> nil) and
         (TextureNode <> Shape.Node.Appearance.Texture) then
      begin
        { This means that Shape.State.MainTexture comes
          from CommonSurfaceShader or X3Dv4 Material or PhysicalMaterial.
          Make sure to still enable shadow maps from Shape.Appearance.Texture
          then.
          TODO: shadow maps should be placed in some special slot,
          dedicated for them. }
        EnableShadowMaps(Shape.Node.Appearance.Texture, TexCoordsNeeded, Shader);
      end;

      { If there is special texture like a normalmap, enable it. }
      BumpMappingEnable(Shape, RenderOptions, BoundTextureUnits, TexCoordsNeeded, Shader);
      SurfaceTexturesEnable(Shape, BoundTextureUnits, TexCoordsNeeded, Shader);
    end;

    { Set alpha test state (shader and fixed-function) }
    if AlphaTest then
    begin
      { only calculate AlphaCutoff when AlphaTest, otherwise it is useless }
      AlphaCutoff := GetAlphaCutoff(Shape);
      RenderContext.FixedFunctionAlphaTestEnable(AlphaCutoff);
      Shader.EnableAlphaTest(AlphaCutoff);
    end else
      RenderContext.FixedFunctionAlphaTestDisable;

    { Make active texture 0. This is helpful for rendering code of
      some primitives that do not support multitexturing now
      (inside vrmlmeshrenderer_x3d_text.inc),
      this way they will at least define correct texture coordinates
      for texture unit 0. }

    if (TexCoordsNeeded > 0) and GLFeatures.UseMultiTexturing then
      ActiveTexture(0);
  end;

  procedure RenderTexturesEnd;
  var
    I: Integer;
  begin
    for I := 0 to Integer(TexCoordsNeeded) - 1 do
      DisableTexture(I);
  end;

begin
  RenderTexturesBegin;
  try
    RenderShapeInside(Shape, RenderOptions, Shader, Lighting, GeneratorClass,
      TMeshRenderer(ExposedMeshRenderer));
  finally RenderTexturesEnd end;
end;

procedure TRenderer.RenderShapeInside(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject);

  procedure SetupShadowMapsGenerationShaders(const MeshRenderer: TMeshRenderer);
  begin
    case RenderingCamera.Target of
      rtVarianceShadowMap:
        begin
          if VarianceShadowMapsShaders = nil then
            VarianceShadowMapsShaders := TCustomShaders.Create(
              '#define VARIANCE_SHADOW_MAPS' + NL + {$I shadow_map_generate.vs.inc},
              '#define VARIANCE_SHADOW_MAPS' + NL + {$I shadow_map_generate.fs.inc});
          MeshRenderer.CustomShaders := VarianceShadowMapsShaders;
        end;
      rtShadowMap:
        begin
          if ShadowMapsShaders = nil then
            ShadowMapsShaders := TCustomShaders.Create(
              {$I shadow_map_generate.vs.inc},
              {$I shadow_map_generate.fs.inc});
          MeshRenderer.CustomShaders := ShadowMapsShaders;
        end;
      else
        begin
          MeshRenderer.CustomShaders := nil;
        end;
    end;
  end;

var
  Generator: TArraysGenerator;
  CoordinateRenderer: TBaseCoordinateRenderer;
begin
  { No point preparing VBO for clones, clones will need own VBOs anyway. }
  if RenderMode = rmPrepareRenderClones then
    Exit;

  { initialize TBaseCoordinateRenderer.Arrays now }
  if GeneratorClass <> nil then
  begin
    Assert(TMeshRenderer(ExposedMeshRenderer) is TBaseCoordinateRenderer);
    CoordinateRenderer := TBaseCoordinateRenderer(ExposedMeshRenderer);

    { calculate Shape.Cache }
    if Shape.Cache = nil then
      Shape.Cache := RendererCache.Shape_IncReference(Shape, RenderOptions);

    { calculate Shape.Cache.Arrays }
    if (Shape.Cache.Arrays = nil) or
       (Shape.Cache.VboToReload <> []) then
    begin
      if not FastUpdateArrays(Shape) then
      begin
        FreeAndNil(Shape.Cache.Arrays); // we just need to create new Arrays
        Generator := GeneratorClass.Create(Shape);
        try
          Generator.TexCoordsNeeded := TexCoordsNeeded;
          Generator.FogVolumetric := FogVolumetric;
          Generator.FogVolumetricDirection := FogVolumetricDirection;
          Generator.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
          Generator.ShapeBumpMappingUsed := Shape.BumpMappingUsed;
          Generator.ShapeBumpMappingTextureCoordinatesId := Shape.BumpMappingTextureCoordinatesId;
          Shape.Cache.Arrays := Generator.GenerateArrays;
        finally FreeAndNil(Generator) end;

        { Always after recreating Shape.Cache.Arrays, reload Shape.Cache.Vbo contents.
          This also clears VboToReload. }
        Shape.LoadArraysToVbo;
      end;
    end;

    if GLFeatures.VertexBufferObject then
    begin
      { Shape.Arrays contents are already loaded,
        so Vbo contents are already loaded too }
      Assert(Shape.Cache.Vbo[vtCoordinate] <> 0);
      CoordinateRenderer.Vbo := Shape.Cache.Vbo;
      CoordinateRenderer.Vao := Shape.Cache.Vao;
    end;

    CoordinateRenderer.Arrays := Shape.Cache.Arrays;
    CoordinateRenderer.Shader := Shader;
    CoordinateRenderer.BoundTextureUnits := BoundTextureUnits;
    CoordinateRenderer.Lighting := Lighting;
  end;

  SetupShadowMapsGenerationShaders(TMeshRenderer(ExposedMeshRenderer));

  TMeshRenderer(ExposedMeshRenderer).PrepareRenderShape := RenderMode in [rmPrepareRenderSelf, rmPrepareRenderClones];
  TMeshRenderer(ExposedMeshRenderer).Render;

  if (GeneratorClass <> nil) and GLFeatures.VertexBufferObject then
  begin
    { unbind arrays, to have a clean state on exit.
      TODO: this should not be needed, instead move to RenderEnd.
      Check does occlusion query work Ok when some vbo is bound. }
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
end;

{$ifndef OpenGLES}
procedure TRenderer.PushTextureUnit(const TexUnit: Cardinal);
begin
  { Only continue if texture unit is not already pushed
    (otherwise glPushMatrix would not be paired by exactly one glPopMatrix
    later). }

  if (TexUnit >= TextureTransformUnitsUsed) and
     (TextureTransformUnitsUsedMore.IndexOf(TexUnit) = -1) then
  begin
    if GLFeatures.EnableFixedFunction then
      glPushMatrix;

    { Simple implementation would just add always TexUnit
      to TextureTransformUnitsUsedMore. But there are optimizations possible,
      knowing that TextureTransformUnitsUsed already takes care of many units,
      and TextureTransformUnitsUsed can only be increased (by this
      very method...) in RenderShape.

      If texture unit is = TextureTransformUnitsUsed, this can be taken care
      of easily, just increase TextureTransformUnitsUsed. (This is an often
      case, as it happens when no texture transform was explicitly defined
      in VRML file, and only one texture unit using WORLDSPACEREFLECTIONVECTOR
      is defined; this is the most common case when using cube env mapping
      with WORLDSPACEREFLECTIONVECTOR.)

      Otherwise, we know (from previous checks) that
      TexUnit > TextureTransformUnitsUsed and it's not mentioned in
      TextureTransformUnitsUsedMore. So add it there. }

    if TexUnit = TextureTransformUnitsUsed then
      Inc(TextureTransformUnitsUsed) else
      TextureTransformUnitsUsedMore.Add(TexUnit);
  end;
end;
{$endif}

procedure TRenderer.UpdatePass;

  { Combine a set of numbers (each in their own range) into one unique number.
    This is like combining a couple of digits into a whole number,
    but each digit is in a separate numeric system.

    This is used to calculate TTotalRenderingPass from a couple of numbers.
    The goal is that changing *any* number must also change the result,
    so that result is a unique representation of all the numbers. }
  function GetTotalPass(
    const Digits: array of Cardinal;
    const Ranges: array of Cardinal): Cardinal;
  var
    I: Integer;
    Multiplier: Cardinal;
  begin
    Result := 0;
    Multiplier := 1;
    Assert(Length(Digits) = Length(Ranges));
    for I := 0 to Length(Digits) - 1 do
    begin
      Result := Result + Digits[I] * Multiplier;
      Multiplier := Multiplier * Ranges[I];
    end;
  end;

begin
  FPass := GetTotalPass(
    [     FInternalPass,       FWireframePass,       FUserPass ],
    [High(FInternalPass), High(FWireframePass), High(FUserPass)]);
end;

procedure TRenderer.SetWireframePass(const Value: TWireframeRenderingPass);
begin
  if FWireframePass <> Value then
  begin
    FWireframePass := Value;
    UpdatePass;
  end;
end;

{$endif}
