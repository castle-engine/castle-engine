{%MainUnit castlerenderoptions.pas}

{
  Copyright 2002-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TRenderOptions = class;

  TRenderOptionsEvent = procedure (Options: TRenderOptions) of object;

  { Options that control rendering,
    available at every scene through @link(TCastleScene.RenderOptions). }
  TRenderOptions = class(TComponent)
  strict private
    FLighting: boolean;
    FUseSceneLights: boolean;
    FOpacity: Single;
    FEnableTextures: boolean;
    FMinificationFilter: TAutoMinificationFilter;
    FMagnificationFilter: TAutoMagnificationFilter;
    FPointSize: Single;
    FLineWidth: Single;
    FBumpMapping: TBumpMapping;
    FCustomShader, FCustomShaderAlphaTest: TObject;
    FMode: TRenderingMode;
    FShadowSampling: TShadowSampling;
    FVisualizeDepthMap: boolean;
    FDepthTest: boolean;
    FPhongShading: boolean;
    FSolidColor: TCastleColorRGB;
    FSolidColorBlendingPipeline: Boolean;
    FSeparateDiffuseTexture: boolean;
    FMaxLightsPerShape: Cardinal;
    FBlending: boolean;
    FBlendingSourceFactor: TBlendingSourceFactor;
    FBlendingDestinationFactor: TBlendingDestinationFactor;
    FBlendingSort: TBlendingSort;
    FOcclusionSort: boolean;
    FControlBlending: boolean;
    FWireframeColor: TCastleColorRGB;
    FWireframeEffect: TWireframeEffect;
    FUseOcclusionQuery: boolean;
    FUseHierarchicalOcclusionQuery: boolean;
    FDebugHierOcclusionQueryResults: boolean;
    FSolidWireframeScale: Single;
    FSolidWireframeBias: Single;
    FSilhouetteScale: Single;
    FSilhouetteBias: Single;

    function GetShaders: TShadersRendering;
    procedure SetShaders(const Value: TShadersRendering);
    { These methods just set the value on given property,
      eventually (some of them) calling ReleaseCachedResources.
      @groupBegin }
    procedure SetEnableTextures(const Value: boolean);
    procedure SetMinificationFilter(const Value: TAutoMinificationFilter);
    procedure SetMagnificationFilter(const Value: TAutoMagnificationFilter);
    procedure SetBumpMapping(const Value: TBumpMapping);
    procedure SetMode(const Value: TRenderingMode);
    procedure SetShadowSampling(const Value: TShadowSampling);
    procedure SetVisualizeDepthMap(const Value: boolean);
    procedure SetBlending(const Value: boolean);
    procedure SetBlendingSourceFactor(const Value: TBlendingSourceFactor);
    procedure SetBlendingDestinationFactor(const Value: TBlendingDestinationFactor);
    procedure SetBlendingSort(const Value: TBlendingSort);
    procedure SetControlBlending(const Value: boolean);
    procedure SetUseOcclusionQuery(const Value: boolean);
    procedure SetPhongShading(const Value: boolean);
    { @groupEnd }
  protected
    { Called before changing an attribute that requires the release
      of things cached in a renderer. This includes attributes that affect:

      @unorderedList(
        @item(How TShapeCache.Arrays contents are generated.
          For example, Generator uses TexCoordsNeeded, so changing
          any attribute that affects TexCoordsNeeded calls this method.)

        @item(How (and if) TShapeCache.Vbo are loaded.)

        @item(How textures are loaded (texture filtering options affect them).)
      ) }
    procedure ReleaseCachedResources; virtual;
  public
    const
      DefaultPointSize = 3.0;
      DefaultLineWidth = 2.0;
      DefaultBumpMapping = bmSteepParallaxShadowing;
      DefaultPhongShading = false;
      DefaultMaxLightsPerShape = 8;

      { }
      DefaultBlendingSourceFactor = bsSrcAlpha;

      { Default value of Attributes.BlendingDestinationFactor.
        See TRenderOptions.BlendingDestinationFactor.

        Using bdOneMinusSrcAlpha is the standard value for 3D graphic stuff,
        often producing best results. However, it causes troubles when
        multiple transparent shapes are visible on the same screen pixel.
        For closed convex 3D objects, using backface culling
        (solid = TRUE for geometry) helps. For multiple transparent shapes,
        sorting the transparent shapes helps,
        see @link(TRenderOptions.BlendingSort).
        Sometimes, no solution works for all camera angles.

        Another disadvantage of bdOneMinusSrcAlpha may be that
        the color of opaque shapes disappears too quickly from
        resulting image (since bdOneMinusSrcAlpha scales it down).
        So the image may be darker than you like.

        You can instead consider using bdOne, that doesn't require sorting
        and never has problems with multiple transparent shapes.
        On the other hand, it only adds to the color,
        often making too bright results. }
      DefaultBlendingDestinationFactor = bdOneMinusSrcAlpha;

      { Default value of @link(TRenderOptions.BlendingSort). }
      DefaultBlendingSort = bs3D;

      DefaultWireframeColor: TCastleColorRGB = (Data: (0, 0, 0));

      DefaultSolidWireframeScale = 1;
      DefaultSolidWireframeBias = 1;
      DefaultSilhouetteScale = 5;
      DefaultSilhouetteBias = 5;

      DefaultShadowSampling = ssPCF16;

    class var
      { Value used when @link(MinificationFilter) is minDefault.
        By default, this is minLinearMipmapLinear. }
      DefaultMinificationFilter: TMinificationFilter;
      { Value used when @link(MagnificationFilter) is magDefault.
        By default, this is magLinear. }
      DefaultMagnificationFilter: TMagnificationFilter;
      { Adjust attributes of all loaded resources. }
      OnCreate: TRenderOptionsEvent;

    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;

    { Is the second TRenderOptions instance on all fields
      that affect TShapeCache, that is things that affect generated geometry
      arrays or vbo. This compares the subset of variables that call
      ReleaseCachedResources --- only the ones that affect TShapeCache. }
    function EqualForShapeCache(SecondValue: TRenderOptions): boolean; virtual;

    { Enable OpenGL lighting when rendering.
      This is @true by default, since it's almost always wanted.

      When Lighting is @false, we disable OpenGL lighting.
      (We had previously a different approach, when we left GL_LIGHTING
      untouched and caller could enable/disable it. But this doesn't really
      work for modern OpenGL, the renderer really has to know if lighting
      is enabled. (to generate proper shaders, and to avoid clumsy
      glPushAttrib / glPopAttrib at some places).) }
    property Lighting: boolean
      read FLighting write FLighting default true;

    { Should we setup VRML/X3D lights as OpenGL lights during rendering.

      VRML/X3D lights are loaded into OpenGL lights. All OpenGL lights
      are always used (we always start from the first OpenGL light 0,
      up to the last available OpenGL light --- this is necessary,
      as shader pipeline must know all the lights anyway).

      Initial OpenGL lights are reserved for BaseLights
      (useful for you to define any lights from outside of the scene).
      Then following OpenGL lights are reserved for the lights defined
      in your scene (if this property is @true).
      The remaining OpenGL lights, if any, are not used (we make sure they
      are disabled for fixed-function pipeline).

      This is independent from the @link(Lighting) property (which merely
      says whether we will turn OpenGL lighting on at all). }
    property UseSceneLights: boolean
      read FUseSceneLights write FUseSceneLights default true;

    { Opacity for all rendered shapes. Setting this to something < 1
      you can make every shape transparent. }
    property Opacity: Single read FOpacity write FOpacity default 1;

    { Take model textures into account. When @true (default),
      then our engine takes care of everything related to texturing
      for you: enabling and using textures for textured parts of the model,
      disabling textures for non-textured parts.

      Otherwise, textures are disabled. }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { Default minification and magnification filters for textures.
      These can be overridden on a per-texture basis in VRML / X3D files
      by X3D TextureProperties node (see X3D specification).

      They can be equal to minDefault, magDefault in which case they
      actually use the values from
      DefaultMinificationFilter, DefaultMagnificationFilter
      (by default minLinearMipmapLinear, magLinear).

      @groupBegin }
    property MinificationFilter: TAutoMinificationFilter
      read FMinificationFilter write SetMinificationFilter default minDefault;
    property MagnificationFilter: TAutoMagnificationFilter
      read FMagnificationFilter write SetMagnificationFilter default magDefault;
    { @groupEnd }

    { Size of points. This has an effect on VRML/X3D PointSet rendering.
      Must be > 0. }
    property PointSize: Single
      read FPointSize write FPointSize default DefaultPointSize;

    { Line width. This has an effect on VRML/X3D LineSet rendering,
      and on wireframe rendering for TRenderOptions.WireframeEffect.
      Must be > 0. }
    property LineWidth: Single
      read FLineWidth write FLineWidth default DefaultLineWidth;

    { Use bump mapping. To actually use this, particular shape must also
      provide normal map (and height map, if you want parallax bump mapping).
      This also requires some OpenGL capabilities, in particular GLSL.

      Simple bump mapping (when only normal map is available)
      means that normals are provided in the texture, and lighting
      is calculated per-fragment.

      Parallax bump mapping means that additionally the texture coordinate
      is perturbed, based on height map and camera direction, to create
      illusion of 3D shape instead of flat surface.
      This makes e.g. the bricks on the texture really
      visible as "standing out", in 3D, from the wall. And self-shadowing
      means that these bricks even cast appropriate shadows on each other.

      Steep parallax mapping requires good GPU to work correctly and fast
      enough. }
    property BumpMapping: TBumpMapping
      read FBumpMapping write SetBumpMapping default DefaultBumpMapping;

    { When GLSL shaders are used.
      This is now a deprecated property, better use @link(PhongShading) to determine
      the shading.
      The engine auto-detects whether to use shaders based on OpenGL capabilities,
      particular shape needs (phong shading, bump mapping, shadow maps, compositing shader effects),
      and GLFeatures.EnableFixedFunction. }
    property Shaders: TShadersRendering read GetShaders write SetShaders; deprecated 'use PhongShading';

    { Whether to use Phong shading by default for all shapes.
      Note that each shape may override it by @link(TAbstractShapeNode.Shading) field. }
    property PhongShading: boolean read FPhongShading write SetPhongShading
      default DefaultPhongShading;

    { Custom GLSL shader to use for the whole scene.
      It must be a @link(TX3DShaderProgramBase) instance.
      When this is assigned, @link(Shaders) value is ignored.

      @italic(Avoid using this.) It's not easy to create portable shaders,
      that work both with OpenGL and OpenGLES. Try using "compositing shaders" instead
      https://castle-engine.io/compositing_shaders.php which still allow you
      to write GLSL effects, but they are integrated into standard shader code. }
    property CustomShader: TObject read FCustomShader write FCustomShader;

    { Alternative custom GLSL shader used when alpha test is necessary.
      It must be a @link(TX3DShaderProgramBase) instance.
      Relevant only if CustomShader <> nil.

      @italic(Do not use this.) This is a temporary hack to enable VSM working
      with alpha test. It's not clean, and should not be used for anything else. }
    property CustomShaderAlphaTest: TObject read FCustomShaderAlphaTest write FCustomShaderAlphaTest;

    { Rendering mode, can be used to disable many rendering features at once. }
    property Mode: TRenderingMode read FMode write SetMode default rmFull;

    { Shadow maps sampling. Various approaches result in various quality and speed. }
    property ShadowSampling: TShadowSampling
      read FShadowSampling write SetShadowSampling
      default DefaultShadowSampling;

    { Visualize depths stored in the shadow maps, instead of using them to
      actually make shadow.

      Even without turning this on, VRML author can always activate it
      explicitly for specific lights. For this, you have to use
      @code(X3DLightNode.defaultShadowMap) field,
      and place a GeneratedShadowMap node there. If the
      @code(GeneratedShadowMap.compareMode) is set to @code('NONE'),
      we will always visualize depths of this shadow map.

      Setting this property to @true has the same effect as setting
      compareMode to "NONE" on all (explicit and implicitly created)
      GeneratedShadowMap nodes. }
    property VisualizeDepthMap: boolean
      read FVisualizeDepthMap write SetVisualizeDepthMap default false;

    { By default, we use depth testing to determine which objects are in front
      of the others. This allows to display all 3D content (all TCastleScene
      instances, and all shapes inside them) in any order.

      For very special purposes, you can disable depth testing.
      This means that 3D objects will always be drawn in front of the previous
      ones, in the order in which they are rendered,
      ignoring the contents of the depth buffer. Use only if you know
      what you're doing, if you're sure that the order of rendering will
      always be good. }
    property DepthTest: boolean read FDepthTest write FDepthTest default true;

    { Color used when @link(Mode) is @link(rmSolidColor). }
    property SolidColor: TCastleColorRGB read FSolidColor write FSolidColor;

    { Whether to render shapes as transparent, when @link(Mode) is @link(rmSolidColor). }
    property SolidColorBlendingPipeline: Boolean
      read FSolidColorBlendingPipeline
      write FSolidColorBlendingPipeline;

    { Set to @true to make diffuse texture affect only material diffuse color
      when the shape is lit and shading is Phong.
      This affects both textures from X3D Appearance.texture,
      and textures from CommonSurfaceShader.diffuseTexture.
      This is more correct (following X3D lighting equations),
      and is more impressive (e.g. specular highlights may be better visible,
      as they are not darkened by a dark diffuse texture).

      For historic reasons and for Gouraud shading, by default, this is @false.
      Which means that "diffuse texture" is actually used to multiply
      a complete result of the lighting calculation.
      This is not correct, but it is necessary for Gouraud shading,
      and it is also depended upon by some applications (since the "diffuse texture"
      effectively multiplies all factors, so it also multiplies
      e.g. emissive factor for "pure emissive materials",
      which may be useful sometimes). }
    property SeparateDiffuseTexture: boolean
      read FSeparateDiffuseTexture
      write FSeparateDiffuseTexture default false;
      deprecated 'rendering always behaves as if this was true now, with Phong shading';

    { For efficiency reasons, we only allow a finite number of lights that can affect
      the given shape. You can increase this number if necessary.

      Note that, instead of increasing this limit,
      you can sometimes adjust your models to fit within this limit.
      Do this using light source radius and/or scope (e.g. you can use "radius" in Blender,
      it is exported OK to glTF), and make smaller shapes.

      Note that on ancient dekstops, with fixed-function OpenGL pipeline,
      there is an additional hard limit (dependent on GPU, but usually 8, for this).
      But on modern desktops, as well as mobile and other platforms, you can increase this limit
      freely. }
    property MaxLightsPerShape: Cardinal
      read FMaxLightsPerShape write FMaxLightsPerShape default DefaultMaxLightsPerShape;

    { Render partially transparent objects.

      More precisely: if this is @true, all shapes with
      transparent materials or textures with non-trivial (not only yes/no)
      alpha channel will be rendered using OpenGL blending
      (with depth test off, like they should for OpenGL).

      If this attribute is @false, everything will be rendered as opaque. }
    property Blending: boolean
      read FBlending write SetBlending default true;

    { Blending function parameters, used when @link(Blending).
      Note that this is only a default, VRML/X3D model can override this
      for specific shapes by using our extension BlendMode node.
      See [https://castle-engine.io/x3d_extensions.php#section_ext_blending].
      @groupBegin }
    property BlendingSourceFactor: TBlendingSourceFactor
      read FBlendingSourceFactor write SetBlendingSourceFactor
      default DefaultBlendingSourceFactor;
    property BlendingDestinationFactor: TBlendingDestinationFactor
      read FBlendingDestinationFactor write SetBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;
    { @groupEnd }

    { How to sort the rendered objects using blending (partial transparency).
      See the @link(TBlendingSort) documentation for possible values.

      This may be overridden in a specific 3D models
      by using NavigationInfo node with blendingSort field,
      see TNavigationInfoNode.BlendingSort. }
    property BlendingSort: TBlendingSort
      read FBlendingSort write SetBlendingSort
      default DefaultBlendingSort;

    { Sort the opaque objects when rendering.
      This may generate speedup on some scenes. }
    property OcclusionSort: boolean read FOcclusionSort write FOcclusionSort;

    { Setting this to @false disables any modification of OpenGL
      blending (and depth mask) state by TCastleScene.
      This makes every other @link(Blending) setting ignored,
      and is useful only if you set your own OpenGL blending parameters
      when rendering this scene. }
    property ControlBlending: boolean
      read FControlBlending write SetControlBlending default true;

    { You can use this to turn on some effects related to rendering model
      in special modes.

      When this is weNormal (default), nothing special is
      done, which means that model polygons are simply passed to OpenGL.
      Whether this results in filled or wireframe, depends on OpenGL
      glPolygonMode setting, filled by default.

      How the wireframe effects work when Mode = rmDepth is undefined now.
      Just don't use Mode = rmDepth if you're unsure.

      See description of TWireframeEffect for what other modes do. }
    property WireframeEffect: TWireframeEffect
      read FWireframeEffect write FWireframeEffect default weNormal;

    property SolidWireframeScale: Single read FSolidWireframeScale write FSolidWireframeScale default DefaultSolidWireframeScale;
    property SolidWireframeBias: Single read FSolidWireframeBias write FSolidWireframeBias default DefaultSolidWireframeBias;
    property SilhouetteScale: Single read FSilhouetteScale write FSilhouetteScale default DefaultSilhouetteScale;
    property SilhouetteBias: Single read FSilhouetteBias write FSilhouetteBias default DefaultSilhouetteBias;

    { Wireframe color, used with some WireframeEffect values.
      Default value is DefaultWireframeColor. }
    property WireframeColor: TCastleColorRGB
      read FWireframeColor write FWireframeColor;

    { Should we use ARB_occlusion_query (if available) to avoid rendering
      shapes that didn't pass occlusion test in previous frame.
      Ignored if GPU doesn't support ARB_occlusion_query.

      @true may give you a large speedup in some scenes.
      OTOH, a lag of one frame may happen between an object should
      be rendered and it actually appears.

      When you render more than once the same instance of TCastleScene scene,
      you should not activate it (as the occlusion query doesn't make sense
      if each following render of the scene takes place at totally different
      translation). Also, when rendering something more than just
      one TCastleScene scene (maybe many times the same TCastleScene instance,
      maybe many different TCastleScene instances, maybe some other
      3D objects) you should try to sort rendering order
      from the most to the least possible occluder (otherwise occlusion
      query will not be as efficient at culling).

      This is ignored if UseHierarchicalOcclusionQuery. }
    property UseOcclusionQuery: boolean
      read FUseOcclusionQuery write SetUseOcclusionQuery default false;

    { Should we use ARB_occlusion_query (if available) with
      a hierarchical algorithm  to avoid rendering
      shapes that didn't pass occlusion test in previous frame.
      Ignored if GPU doesn't support ARB_occlusion_query.

      @true may give you a large speedup in some scenes.

      This method doesn't impose any lag of one frame (like UseOcclusionQuery).

      This requires the usage of ssRendering in TCastleSceneCore.Spatial.
      Also, it always does frustum culling (like fcBox for now),
      regardless of TCastleScene.OctreeFrustumCulling setting.

      The algorithm used underneath is "Coherent Hierarchical Culling",
      described in detail in "GPU Gems 2",
      Chapter 6: "Hardware Occlusion Queries Made Useful",
      by Michael Wimmer and Jiri Bittner. Online on
      [http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html].

      @exclude
      @bold(Experimental):
      Using the "Hierarchical Occlusion Query" is not adviced in the current implementation,
      it is slow and it does not treat transparent shapes correctly.
    }
    property UseHierarchicalOcclusionQuery: boolean
      read FUseHierarchicalOcclusionQuery
      write FUseHierarchicalOcclusionQuery default false;
      experimental;

    { View only the shapes that were detected as visible by occlusion query
      in last Render.

      Use this only after render with UseHierarchicalOcclusionQuery.
      TODO: for UseOcclusionQuery I would also like to make it work,
      for now not done as frustum information is gone.
      This will disable actual occlusion query,
      instead reusing results from last occlusion
      query done when this debug flag was @false.

      Useful to quickly visualize the benefits of occlusion query. }
    property DebugHierOcclusionQueryResults: boolean
      read FDebugHierOcclusionQueryResults
      write FDebugHierOcclusionQueryResults default false;
  end;

  TRenderOptionsClass = class of TRenderOptions;

{$endif read_interface}

{$ifdef read_implementation}

{ TRenderOptions --------------------------------------------------- }

constructor TRenderOptions.Create(AOwner: TComponent);
begin
  inherited;

  FLighting := true;
  FUseSceneLights := true;
  FOpacity := 1;
  FEnableTextures := true;
  FMinificationFilter := minDefault;
  FMagnificationFilter := magDefault;
  FPointSize := DefaultPointSize;
  FLineWidth := DefaultLineWidth;
  FBumpMapping := DefaultBumpMapping;
  FShadowSampling := DefaultShadowSampling;
  FDepthTest := true;
  FPhongShading := DefaultPhongShading;
  FMaxLightsPerShape := DefaultMaxLightsPerShape;
  FBlending := true;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;
  FBlendingSort := DefaultBlendingSort;
  FOcclusionSort := false;
  FControlBlending := true;
  FSolidWireframeScale := DefaultSolidWireframeScale;
  FSolidWireframeBias := DefaultSolidWireframeBias;
  FSilhouetteScale := DefaultSilhouetteScale;
  FSilhouetteBias := DefaultSilhouetteBias;
  FWireframeEffect := weNormal;
  FWireframeColor := DefaultWireframeColor;

  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TRenderOptions.Assign(Source: TPersistent);
begin
  if Source is TRenderOptions then
  begin
    Lighting := TRenderOptions(Source).Lighting;
    UseSceneLights := TRenderOptions(Source).UseSceneLights;
    Opacity := TRenderOptions(Source).Opacity;
    EnableTextures := TRenderOptions(Source).EnableTextures;
    MinificationFilter := TRenderOptions(Source).MinificationFilter;
    MagnificationFilter := TRenderOptions(Source).MagnificationFilter;
    PointSize := TRenderOptions(Source).PointSize;
    LineWidth := TRenderOptions(Source).LineWidth;
    Blending := TRenderOptions(Source).Blending;
    BlendingSourceFactor := TRenderOptions(Source).BlendingSourceFactor;
    BlendingDestinationFactor := TRenderOptions(Source).BlendingDestinationFactor;
    BlendingSort := TRenderOptions(Source).BlendingSort;
    OcclusionSort := TRenderOptions(Source).OcclusionSort;
    ControlBlending := TRenderOptions(Source).ControlBlending;
    UseOcclusionQuery := TRenderOptions(Source).UseOcclusionQuery;
    {$warnings off}
    UseHierarchicalOcclusionQuery := TRenderOptions(Source).UseHierarchicalOcclusionQuery;
    {$warnings on}
  end else
    inherited;
end;

function TRenderOptions.EqualForShapeCache(
  SecondValue: TRenderOptions): boolean;
begin
  Result :=
    (SecondValue.EnableTextures = EnableTextures);
end;

procedure TRenderOptions.ReleaseCachedResources;
begin
end;

procedure TRenderOptions.SetEnableTextures(const Value: boolean);
begin
  if EnableTextures <> Value then
  begin
    ReleaseCachedResources;
    FEnableTextures := Value;
  end;
end;

procedure TRenderOptions.SetMinificationFilter(const Value: TAutoMinificationFilter);
begin
  if MinificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMinificationFilter := Value;
  end;
end;

procedure TRenderOptions.SetMagnificationFilter(const Value: TAutoMagnificationFilter);
begin
  if MagnificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMagnificationFilter := Value;
  end;
end;

procedure TRenderOptions.SetBumpMapping(const Value: TBumpMapping);
begin
  if BumpMapping <> Value then
  begin
    ReleaseCachedResources;
    FBumpMapping := Value;
  end;
end;

procedure TRenderOptions.SetMode(const Value: TRenderingMode);
begin
  FMode := Value;
end;

procedure TRenderOptions.SetShadowSampling(const Value: TShadowSampling);
begin
  if FShadowSampling <> Value then
  begin
    { When swithing between VSM and non-VSM sampling methods,
      we need to ReleaseCachedResources, since shadow maps must be regenerated. }
    if (FShadowSampling = ssVarianceShadowMaps) <>
       (Value           = ssVarianceShadowMaps) then
      ReleaseCachedResources;

    FShadowSampling := Value;
  end;
end;

procedure TRenderOptions.SetVisualizeDepthMap(const Value: boolean);
begin
  if VisualizeDepthMap <> Value then
  begin
    ReleaseCachedResources;
    FVisualizeDepthMap := Value;
  end;
end;

function TRenderOptions.GetShaders: TShadersRendering;
begin
  if PhongShading then
    Result := srAlways
  else
    Result := srWhenRequired;
end;

procedure TRenderOptions.SetShaders(const Value: TShadersRendering);
begin
  PhongShading := Value = srAlways;
end;

procedure TRenderOptions.SetBlending(const Value: boolean);
begin
  FBlending := Value;
end;

procedure TRenderOptions.SetBlendingSourceFactor(
  const Value: TBlendingSourceFactor);
begin
  FBlendingSourceFactor := Value;
end;

procedure TRenderOptions.SetBlendingDestinationFactor(
  const Value: TBlendingDestinationFactor);
begin
  FBlendingDestinationFactor := Value;
end;

procedure TRenderOptions.SetBlendingSort(const Value: TBlendingSort);
begin
  FBlendingSort := Value;
end;

procedure TRenderOptions.SetControlBlending(const Value: boolean);
begin
  FControlBlending := Value;
end;

procedure TRenderOptions.SetUseOcclusionQuery(const Value: boolean);
begin
  if UseOcclusionQuery <> Value then
  begin
    FUseOcclusionQuery := Value;

    if UseOcclusionQuery then
    begin
      { Make PrepareShapesResources again, to cause TGLShape.PrepareResources
        that initializes OcclusionQueryId for each shape }
      ReleaseCachedResources;
    end;
  end;
end;

procedure TRenderOptions.SetPhongShading(const Value: boolean);
begin
  if PhongShading <> Value then
  begin
    FPhongShading := Value;
    { When switching this we want to force generating necessary
      shaders at the next PrepareResources call. Otherwise shaders would
      be prepared only when shapes come into view, which means that navigating
      awfully stutters for some time after changing this property.

      We could do this by OwnerScene.PreparedRender := false.
      Or just ReleaseCachedResources. }
    ReleaseCachedResources;
  end;
end;

{$endif read_implementation}
