{%MainUnit castlerenderoptions.pas}
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

{$ifdef read_interface}

type
  TCastleRenderOptions = class;

  TCastleRenderOptionsEvent = procedure (const Options: TCastleRenderOptions) of object;

  { Options that control rendering,
    available at every scene through @link(TCastleScene.RenderOptions). }
  TCastleRenderOptions = class(TComponent)
  strict private
    FLighting: boolean;
    FReceiveSceneLights, FReceiveGlobalLights: boolean;
    FTextures: boolean;
    FMinificationFilter: TAutoMinificationFilter;
    FMagnificationFilter: TAutoMagnificationFilter;
    FPointSize: Single;
    FLineWidth: Single;
    FBumpMapping: TBumpMapping;
    FBumpMappingParallaxHeight: Single;
    FInternalCustomShader, FInternalCustomShaderAlphaTest: TObject;
    FMode: TRenderingMode;
    FShadowSampling: TShadowSampling;
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
    FWireframeColor: TCastleColorRGB;
    FWireframeEffect: TWireframeEffect;
    FOcclusionQuery: boolean;
    FHierarchicalOcclusionQuery: boolean;
    FDebugHierOcclusionQueryResults: boolean;
    FSolidWireframeScale: Single;
    FSolidWireframeBias: Single;
    FSilhouetteScale: Single;
    FSilhouetteBias: Single;
    FRobustNegativeScale: Boolean;
    FWholeSceneManifold: Boolean;

    function GetShaders: TShadersRendering;
    procedure SetShaders(const Value: TShadersRendering);
    { These methods just set the value on given property,
      eventually (some of them) calling ReleaseCachedResources.
      @groupBegin }
    procedure SetTextures(const Value: boolean);
    procedure SetMinificationFilter(const Value: TAutoMinificationFilter);
    procedure SetMagnificationFilter(const Value: TAutoMagnificationFilter);
    procedure SetBumpMapping(const Value: TBumpMapping);
    procedure SetMode(const Value: TRenderingMode);
    procedure SetShadowSampling(const Value: TShadowSampling);
    procedure SetBlending(const Value: boolean);
    procedure SetBlendingSourceFactor(const Value: TBlendingSourceFactor);
    procedure SetBlendingDestinationFactor(const Value: TBlendingDestinationFactor);
    procedure SetBlendingSort(const Value: TBlendingSort);
    procedure SetOcclusionQuery(const Value: boolean);
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
      DefaultBumpMapping = bmBasic;
      DefaultBumpMappingParallaxHeight = 0.01;
      DefaultPhongShading = true;
      DefaultMaxLightsPerShape = 64;

      { }
      DefaultBlendingSourceFactor = bsSrcAlpha;

      { Default value of Attributes.BlendingDestinationFactor.
        See TCastleRenderOptions.BlendingDestinationFactor.

        Using bdOneMinusSrcAlpha is the standard value for 3D graphic stuff,
        often producing best results. However, it causes troubles when
        multiple transparent shapes are visible on the same screen pixel.
        For closed convex 3D objects, using backface culling
        (solid = TRUE for geometry) helps. For multiple transparent shapes,
        sorting the transparent shapes helps,
        see @link(TCastleRenderOptions.BlendingSort).
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

      { Default value of @link(TCastleRenderOptions.BlendingSort). }
      DefaultBlendingSort = bs3D;

      DefaultWireframeColor: TCastleColorRGB = (X: 0; Y: 0; Z: 0);

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
      OnCreate: TCastleRenderOptionsEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    { Is the second TCastleRenderOptions instance on all fields
      that affect TShapeCache, that is things that affect generated geometry
      arrays or vbo. This compares the subset of variables that call
      ReleaseCachedResources --- only the ones that affect TShapeCache. }
    function EqualForShapeCache(SecondValue: TCastleRenderOptions): boolean; virtual;

    {$ifdef FPC}
    property EnableTextures: boolean
      read FTextures write SetTextures default true; deprecated 'use Textures';

    { When GLSL shaders are used.
      This is now a deprecated property, better use @link(PhongShading) to determine
      the shading.
      The engine auto-detects whether to use shaders based on OpenGL capabilities,
      particular shape needs (phong shading, bump mapping, shadow maps, compositing shader effects),
      and GLFeatures.EnableFixedFunction. }
    property Shaders: TShadersRendering read GetShaders write SetShaders; deprecated 'use PhongShading';
    {$endif}

    { Custom GLSL shader to use for the whole scene.
      It must be a @link(TX3DShaderProgramBase) instance.
      When this is assigned, @link(Shaders) value is ignored.

      @italic(Internal, do not use this.)
      Because TX3DShaderProgramBase is an internal class, because it's cumbersome to use.

      Use nodes instead, to override shaders or to enhance shaders.
      See examples/viewport_and_scenes/shader_effects/ ,
      examples/viewport_and_scenes/shader_override/ .

      @exclude }
    property InternalCustomShader: TObject
      read FInternalCustomShader write FInternalCustomShader;

    { Alternative custom GLSL shader used when alpha test is necessary.
      It must be a @link(TX3DShaderProgramBase) instance.
      Relevant only if CustomShader <> nil.

      @italic(Internal, do not use this.)
      Because TX3DShaderProgramBase is an internal class, because it's cumbersome to use.

      Use nodes instead, to override shaders or to enhance shaders.
      See examples/viewport_and_scenes/shader_effects/ ,
      examples/viewport_and_scenes/shader_override/ .

      This is in practice an internal solution to enable VSM working
      with alpha test. It's not clean, and should not be used for anything else.

      @exclude }
    property InternalCustomShaderAlphaTest: TObject
      read FInternalCustomShaderAlphaTest write FInternalCustomShaderAlphaTest;

    { Rendering mode, can be used to disable many rendering features at once. }
    property Mode: TRenderingMode read FMode write SetMode default rmFull;

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

    {$ifdef FPC}
    { @deprecated @exclude Ignored now.

      Set to @true to make diffuse texture affect only material diffuse color
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
    {$endif FPC}

    { Wireframe color, used with some WireframeEffect values.
      Default value is DefaultWireframeColor. }
    property WireframeColor: TCastleColorRGB
      read FWireframeColor write FWireframeColor;

    { Use the advanced version of the occlusion query:
      "Coherent Hierarchical Culling", to optimize the rendered shapes.
      The shapes obscured by other shapes will not be rendered.

      This is ignored if GPU doesn't support the necessary functionality
      (TGLFeatures.OcclusionQuery).

      We don't advise this approach to occlusion query now.
      While this avoids the problem of "lag of one frame" (compared to @link(OcclusionQuery))
      when the object should appear...
      But the resulting algorithm (at least in our current implementation)
      is slow and it does not treat transparent shapes correctly.

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
    property HierarchicalOcclusionQuery: boolean
      read FHierarchicalOcclusionQuery
      write FHierarchicalOcclusionQuery default false;
      {$ifdef FPC}experimental;{$endif FPC}

    { View only the shapes that were detected as visible by occlusion query
      in last Render.

      Use this only after render with HierarchicalOcclusionQuery.
      TODO: for OcclusionQuery I would also like to make it work,
      for now not done as frustum information is gone.
      This will disable actual occlusion query,
      instead reusing results from last occlusion
      query done when this debug flag was @false.

      Useful to quickly visualize the benefits of occlusion query. }
    property DebugHierOcclusionQueryResults: boolean
      read FDebugHierOcclusionQueryResults
      write FDebugHierOcclusionQueryResults default false;

    { @deprecated @exclude Old name for @link(ReceiveSceneLights). }
    property UseSceneLights: boolean
      read FReceiveSceneLights write FReceiveSceneLights default true;
      {$ifdef FPC}deprecated 'use ReceiveSceneLights';{$endif}

    { @deprecated @exclude Old name for @link(ReceiveSceneLights). }
    property SceneLights: boolean
      read FReceiveSceneLights write FReceiveSceneLights default true;
      {$ifdef FPC}deprecated 'use ReceiveSceneLights';{$endif}

    { @deprecated @exclude Old name for @link(OcclusionQuery). }
    property UseOcclusionQuery: boolean
      read FOcclusionQuery write SetOcclusionQuery default false;
      {$ifdef FPC}deprecated 'use OcclusionQuery';{$endif}
  published
    { Enable real-time lighting when rendering. }
    property Lighting: boolean read FLighting write FLighting default true;

    { Use lights defined in this scene (in the model loaded to this TCastleScene)
      to light the shapes in this scene.

      Note: This property controls whether lights defined within a model (TCastleScene)
      affect the shapes in the same model.
      In case of such lights, the value of @link(TCastleScene.CastGlobalLights)
      and @link(ReceiveGlobalLights) doesn't matter. Only this property, ReceiveSceneLights, controls
      whether such lights work.

      The other lights are controlled by @link(ReceiveGlobalLights).

      This doesn't matter if @link(Lighting) is @false.
      Also, if both ReceiveSceneLights and ReceiveGlobalLights are @false,
      then the scene receives no lighting at all. }
    property ReceiveSceneLights: boolean
      read FReceiveSceneLights write FReceiveSceneLights default true;

    { Use lights defined in other scenes to light the shapes in this scene.

      This property controls whether @italic(other) scenes with
      @link(TCastleScene.CastGlobalLights),
      as well as headlight, shine on this scene.

      This doesn't matter if @link(Lighting) is @false.
      Also, if both ReceiveSceneLights and ReceiveGlobalLights are @false,
      then the scene receives no lighting at all. }
    property ReceiveGlobalLights: boolean
      read FReceiveGlobalLights write FReceiveGlobalLights default true;

    { Use textures. }
    property Textures: boolean
      read FTextures write SetTextures default true;

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

    { Size of points. This has an effect on TPointSetNode rendering.
      Must be > 0. }
    property PointSize: Single
      read FPointSize write FPointSize {$ifdef FPC}default DefaultPointSize{$endif};

    { Line width. This has an effect on TLineSetNode rendering,
      and on wireframe rendering when @link(TCastleRenderOptions.WireframeEffect) indicates it.
      Must be > 0. }
    property LineWidth: Single
      read FLineWidth write FLineWidth {$ifdef FPC}default DefaultLineWidth{$endif};

    { Use bump mapping. The bump mapping is only done when particular shape
      defines a normal map (and a height map, if you want parallax bump mapping).
      See https://castle-engine.io/bump_mapping .

      The normal map is usually provided in
      the @link(TAbstractOneSidedMaterialNode.NormalTexture) field.
      @link(TAbstractOneSidedMaterialNode) is an ancestor of all useful
      material nodes, like @link(TMaterialNode) (Phong lighting),
      @link(TPhysicalMaterialNode) (PBR lighting), and even
      @link(TUnlitMaterialNode) (unlit -- doesn't use normals for lighting,
      but may still use them e.g. for tex coord generation).
      So all material nodes allow to specify normal map.

      See TBumpMapping for various possible values. }
    property BumpMapping: TBumpMapping
      read FBumpMapping write SetBumpMapping default DefaultBumpMapping;

    { Maximum height expressed in the normal map alpha channel,
      used only when @link(BumpMapping) indicates one of the "parallax" options.

      By default this is ignored because @link(BumpMapping) by default is just bmBasic,
      which ignores the height map in the normal map alpha channel.
      If your normal map includes an alpha channel,
      and you set @link(BumpMapping) to a value bmParallax, bmSteepParallax,
      bmSteepParallaxShadowing then this property is used to interpret the height information. }
    property BumpMappingParallaxHeight: Single
      read FBumpMappingParallaxHeight write FBumpMappingParallaxHeight
      {$ifdef FPC}default DefaultBumpMappingParallaxHeight{$endif};

    { Whether to use Phong shading by default.

      Note that each shape may override it by @link(TAbstractShapeNode.Shading) field.

      Note that Phong shading is forced anyway by various situations:

      @unorderedList(
        @itemSpacing Compact
        @item PBR materials (TPhysicalMaterialNode, in particular imported from glTF)
        @item Using textures like normal maps, specular maps
        @item Using shadow maps
      ) }
    property PhongShading: boolean read FPhongShading write SetPhongShading
      default DefaultPhongShading;

    { Shadow maps sampling. Various approaches result in various quality and speed. }
    property ShadowSampling: TShadowSampling
      read FShadowSampling write SetShadowSampling
      default DefaultShadowSampling;

    { For efficiency reasons, we only allow a finite number of lights that can affect
      the given shape.

      You can increase this number if necessary,
      although note that it is alreday quite large by default.
      Instead of increasing this limit,
      it is always more efficient to design your scenes to fit within this limit.
      Use the light source radius and/or scope (e.g. you can use "radius" in Blender,
      it is exported OK to glTF), and make smaller shapes.

      Note that on ancient dekstops, with fixed-function OpenGL pipeline,
      there is an additional hard limit (dependent on GPU, but usually 8, for this).
      But on modern desktops, as well as mobile and other platforms,
      you can increase this limit freely. }
    property MaxLightsPerShape: Cardinal
      read FMaxLightsPerShape write FMaxLightsPerShape default DefaultMaxLightsPerShape;

    { Render partially transparent objects.

      More precisely: if this is @true, all shapes with
      transparent materials or textures with non-trivial (not only yes/no)
      alpha channel will be rendered using blending.
      See https://castle-engine.io/blending
      for details how it works.

      If this is @false, everything will be rendered as opaque. }
    property Blending: boolean
      read FBlending write SetBlending default true;

    { Blending function parameters, used when @link(Blending).
      Note that this is only a default, each model can override this
      for specific shapes using @link(TBlendModeNode).
      See https://castle-engine.io/x3d_extensions.php#section_ext_blending for details of TBlendModeNode.

      See https://castle-engine.io/blending
      for details how it works.

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
    property OcclusionSort: boolean read FOcclusionSort write FOcclusionSort default false;

    { Activate various effects related to wireframe rendering.
      When this is weNormal (default), we simply render polygons as polygons.
      See description of TWireframeEffect for what other modes do.

      Note: How the wireframe effects work when Mode = rmDepth is undefined now.
      Don't use Mode = rmDepth with WireframeEffect <> weNormal. }
    property WireframeEffect: TWireframeEffect
      read FWireframeEffect write FWireframeEffect default weNormal;

    property SolidWireframeScale: Single read FSolidWireframeScale write FSolidWireframeScale {$ifdef FPC}default DefaultSolidWireframeScale{$endif};
    property SolidWireframeBias: Single read FSolidWireframeBias write FSolidWireframeBias {$ifdef FPC}default DefaultSolidWireframeBias{$endif};
    property SilhouetteScale: Single read FSilhouetteScale write FSilhouetteScale {$ifdef FPC}default DefaultSilhouetteScale{$endif};
    property SilhouetteBias: Single read FSilhouetteBias write FSilhouetteBias {$ifdef FPC}default DefaultSilhouetteBias{$endif};

    { Use the occlusion query to optimize the rendering.
      The shapes obscured by other shapes will not be rendered.
      This makes sense when in your view, many shapes are typically obscured by others.

      See the https://castle-engine.io/occlusion_query
      for details how does this work.

      This is ignored if HierarchicalOcclusionQuery.

      This is ignored if GPU doesn't support the necessary functionality
      (@link(TGLFeatures.OcclusionQuery)). }
    property OcclusionQuery: boolean
      read FOcclusionQuery write SetOcclusionQuery default false;

    { Support lighting and backface culling for models using negative scale.
      This has a small performance cost, and so is disabled by default. }
    property RobustNegativeScale: Boolean
      read FRobustNegativeScale write FRobustNegativeScale default false;

    { When considering this scene to be a shadow caster for shadow volumes,
      assume it is 2-manifold when treated as whole scene (each separate
      shape may be not 2-manifold). This means that you guarantee that
      all border edges of all shapes match with each other, closing the skin
      of the model perfectly.
      See https://castle-engine.io/x3d_extensions_shadow_volumes.php
      about using shadow volumes and the requirement to be 2-manifold.

      If this is @true then, even if some shapes are not 2-manifold,
      we will generate shadow (using shadow volumes) from them.
      And we will never reject shadows from a particular shape,
      we will either reject whole scene shadow, or render whole scene shadow.

      It is the user responsibility to guarantee that whole scene is
      really 2-manifold. If it is not, then shadow volumes may exhibit
      weird artifacts. }
    property WholeSceneManifold: Boolean
      read FWholeSceneManifold write FWholeSceneManifold default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlerenderoptions_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleRenderOptionsClass = class of TCastleRenderOptions;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleRenderOptions --------------------------------------------------- }

constructor TCastleRenderOptions.Create(AOwner: TComponent);
begin
  inherited;

  FLighting := true;
  FReceiveSceneLights := true;
  FReceiveGlobalLights := true;
  FTextures := true;
  FMinificationFilter := minDefault;
  FMagnificationFilter := magDefault;
  FPointSize := DefaultPointSize;
  FLineWidth := DefaultLineWidth;
  FBumpMapping := DefaultBumpMapping;
  FBumpMappingParallaxHeight := DefaultBumpMappingParallaxHeight;
  FShadowSampling := DefaultShadowSampling;
  FDepthTest := true;
  FPhongShading := DefaultPhongShading;
  FMaxLightsPerShape := DefaultMaxLightsPerShape;
  FBlending := true;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;
  FBlendingSort := DefaultBlendingSort;
  FOcclusionSort := false;
  FSolidWireframeScale := DefaultSolidWireframeScale;
  FSolidWireframeBias := DefaultSolidWireframeBias;
  FSilhouetteScale := DefaultSilhouetteScale;
  FSilhouetteBias := DefaultSilhouetteBias;
  FWireframeEffect := weNormal;
  FWireframeColor := DefaultWireframeColor;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlerenderoptions_persistent_vectors.inc}
  {$undef read_implementation_constructor}

  if Assigned(OnCreate) then
    OnCreate(Self);
end;

destructor TCastleRenderOptions.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlerenderoptions_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleRenderOptions.Assign(Source: TPersistent);
begin
  if Source is TCastleRenderOptions then
  begin
    Lighting := TCastleRenderOptions(Source).Lighting;
    ReceiveSceneLights := TCastleRenderOptions(Source).ReceiveSceneLights;
    ReceiveGlobalLights := TCastleRenderOptions(Source).ReceiveGlobalLights;
    Textures := TCastleRenderOptions(Source).Textures;
    MinificationFilter := TCastleRenderOptions(Source).MinificationFilter;
    MagnificationFilter := TCastleRenderOptions(Source).MagnificationFilter;
    PointSize := TCastleRenderOptions(Source).PointSize;
    LineWidth := TCastleRenderOptions(Source).LineWidth;
    Blending := TCastleRenderOptions(Source).Blending;
    BlendingSourceFactor := TCastleRenderOptions(Source).BlendingSourceFactor;
    BlendingDestinationFactor := TCastleRenderOptions(Source).BlendingDestinationFactor;
    BlendingSort := TCastleRenderOptions(Source).BlendingSort;
    OcclusionSort := TCastleRenderOptions(Source).OcclusionSort;
    OcclusionQuery := TCastleRenderOptions(Source).OcclusionQuery;
    {$warnings off}
    HierarchicalOcclusionQuery := TCastleRenderOptions(Source).HierarchicalOcclusionQuery;
    {$warnings on}
  end else
    inherited;
end;

function TCastleRenderOptions.EqualForShapeCache(
  SecondValue: TCastleRenderOptions): boolean;
begin
  Result :=
    (SecondValue.Textures = Textures);
end;

procedure TCastleRenderOptions.ReleaseCachedResources;
begin
end;

procedure TCastleRenderOptions.SetTextures(const Value: boolean);
begin
  if Textures <> Value then
  begin
    ReleaseCachedResources;
    FTextures := Value;
  end;
end;

procedure TCastleRenderOptions.SetMinificationFilter(const Value: TAutoMinificationFilter);
begin
  if MinificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMinificationFilter := Value;
  end;
end;

procedure TCastleRenderOptions.SetMagnificationFilter(const Value: TAutoMagnificationFilter);
begin
  if MagnificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMagnificationFilter := Value;
  end;
end;

procedure TCastleRenderOptions.SetBumpMapping(const Value: TBumpMapping);
begin
  if BumpMapping <> Value then
  begin
    ReleaseCachedResources;
    FBumpMapping := Value;
  end;
end;

procedure TCastleRenderOptions.SetMode(const Value: TRenderingMode);
begin
  FMode := Value;
end;

procedure TCastleRenderOptions.SetShadowSampling(const Value: TShadowSampling);
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

function TCastleRenderOptions.GetShaders: TShadersRendering;
begin
  if PhongShading then
    Result := srAlways
  else
    Result := srWhenRequired;
end;

procedure TCastleRenderOptions.SetShaders(const Value: TShadersRendering);
begin
  PhongShading := Value = srAlways;
end;

procedure TCastleRenderOptions.SetBlending(const Value: boolean);
begin
  FBlending := Value;
end;

procedure TCastleRenderOptions.SetBlendingSourceFactor(
  const Value: TBlendingSourceFactor);
begin
  FBlendingSourceFactor := Value;
end;

procedure TCastleRenderOptions.SetBlendingDestinationFactor(
  const Value: TBlendingDestinationFactor);
begin
  FBlendingDestinationFactor := Value;
end;

procedure TCastleRenderOptions.SetBlendingSort(const Value: TBlendingSort);
begin
  FBlendingSort := Value;
end;

procedure TCastleRenderOptions.SetOcclusionQuery(const Value: boolean);
begin
  if OcclusionQuery <> Value then
  begin
    FOcclusionQuery := Value;

    if OcclusionQuery then
    begin
      { Make PrepareShapesResources again, to cause TGLShape.PrepareResources
        that initializes OcclusionQueryId for each shape }
      ReleaseCachedResources;
    end;
  end;
end;

procedure TCastleRenderOptions.SetPhongShading(const Value: boolean);
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

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlerenderoptions_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
