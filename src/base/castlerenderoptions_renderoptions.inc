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
  TCastleRenderOptions = class(TCastleComponent)
  strict private
    type
      TRenderOptionsHash = Int32;
    var
      FLighting: Boolean;
      FReceiveSceneLights, FReceiveGlobalLights: Boolean;
      FTextures: Boolean;
      FMinificationFilter: TAutoMinificationFilter;
      FMagnificationFilter: TAutoMagnificationFilter;
      FPointSize: Single;
      FLineWidth: Single;
      FBumpMapping: TBumpMapping;
      FBumpMappingParallaxHeight: Single;
      FMode: TRenderingMode;
      FShadowSampling: TShadowSampling;
      FDepthTest: Boolean;
      FPhongShading: Boolean;
      FSolidColor: TCastleColorRGB;
      FMaxLightsPerShape: Cardinal;
      FBlending: Boolean;
      FBlendingSourceFactor: TBlendingSourceFactor;
      FBlendingDestinationFactor: TBlendingDestinationFactor;
      FWireframeColor: TCastleColorRGB;
      FWireframeEffect: TWireframeEffect;
      FSolidWireframeScale: Single;
      FSolidWireframeBias: Single;
      FSilhouetteScale: Single;
      FSilhouetteBias: Single;
      FRobustNegativeScale: Boolean;
      FWholeSceneManifold: Boolean;
      FColorChannels: TColorChannels;

    { Hash to quickly compare 2 TCastleRenderOptions instances,
      important for efficient batching. }
    FHash: TRenderOptionsHash;

    // Property setters that
    // - call UpdateHash
    // - sometimes call ReleaseCachedResources
    procedure SetTextures(const Value: Boolean);
    procedure SetMinificationFilter(const Value: TAutoMinificationFilter);
    procedure SetMagnificationFilter(const Value: TAutoMagnificationFilter);
    procedure SetBumpMapping(const Value: TBumpMapping);
    procedure SetMode(const Value: TRenderingMode);
    procedure SetShadowSampling(const Value: TShadowSampling);
    procedure SetBlending(const Value: Boolean);
    procedure SetBlendingSourceFactor(const Value: TBlendingSourceFactor);
    procedure SetBlendingDestinationFactor(const Value: TBlendingDestinationFactor);
    procedure SetPhongShading(const Value: Boolean);
    procedure SetWireframeColor(const Value: TCastleColorRGB);
    procedure SetWireframeEffect(const Value: TWireframeEffect);
    procedure SetSolidWireframeScale(const Value: Single);
    procedure SetSolidWireframeBias(const Value: Single);
    procedure SetSilhouetteScale(const Value: Single);
    procedure SetSilhouetteBias(const Value: Single);
    procedure SetRobustNegativeScale(const Value: Boolean);
    procedure SetWholeSceneManifold(const Value: Boolean);
    procedure SetColorChannels(const Value: TColorChannels);
    procedure SetMaxLightsPerShape(const Value: Cardinal);
    procedure SetDepthTest(const Value: Boolean);
    procedure SetReceiveSceneLights(const Value: Boolean);
    procedure SetReceiveGlobalLights(const Value: Boolean);
    procedure SetSolidColor(const Value: TCastleColorRGB);
    procedure SetPointSize(const Value: Single);
    procedure SetLineWidth(const Value: Single);
    procedure SetBumpMappingParallaxHeight(const Value: Single);
    procedure SetLighting(const Value: Boolean);

    procedure UpdateHash;
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

      { Default value of @link(TCastleRenderOptions.BlendingSourceFactor).
        This is the typical blending operation. }
      DefaultBlendingSourceFactor = bsSrcAlpha;

      { Default value of @link(TCastleRenderOptions.BlendingDestinationFactor).
        This is the typical blending operation. }
      DefaultBlendingDestinationFactor = bdOneMinusSrcAlpha;

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
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Equal TCastleRenderOptions produce the same effect when rendering
      and can be treated as exactly equivalent by the @link(TCastleViewport.DynamicBatching)
      algorithm. }
    function Equals(Obj: TObject): Boolean; override;

    { Is the second TCastleRenderOptions instance on all fields
      that affect TShapeCache, that is things that affect generated geometry
      arrays or vbo. This compares the subset of variables that call
      ReleaseCachedResources --- only the ones that affect TShapeCache. }
    function EqualForShapeCache(SecondValue: TCastleRenderOptions): boolean; virtual;

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
    property DepthTest: boolean read FDepthTest write SetDepthTest default true;

    { Color used when @link(Mode) is @link(rmSolidColor).
      By default BlackRGB. }
    property SolidColor: TCastleColorRGB read FSolidColor write SetSolidColor;

    { Wireframe color, used with some WireframeEffect values.
      Default value is DefaultWireframeColor. }
    property WireframeColor: TCastleColorRGB
      read FWireframeColor write SetWireframeColor;

    { Which color buffer channels (RGBA) to write when rendering this model.

      This makes a cool effect, avoiding writing to some color channels,
      but writing others, and writing depth buffer.
      This property may change to be non-internal some day, please report
      on Castle Game Engine forum / Discord / GitHub
      ( https://castle-engine.io/talk.php ) if you find it useful. }
    property InternalColorChannels: TColorChannels
      read FColorChannels write SetColorChannels default AllColorChannels;
  published
    { Enable real-time lighting when rendering. }
    property Lighting: boolean read FLighting write SetLighting default true;

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
      read FReceiveSceneLights write SetReceiveSceneLights default true;

    { Use lights defined in other scenes to light the shapes in this scene.

      This property controls whether @italic(other) scenes with
      @link(TCastleScene.CastGlobalLights),
      as well as headlight, shine on this scene.

      This doesn't matter if @link(Lighting) is @false.
      Also, if both ReceiveSceneLights and ReceiveGlobalLights are @false,
      then the scene receives no lighting at all. }
    property ReceiveGlobalLights: boolean
      read FReceiveGlobalLights write SetReceiveGlobalLights default true;

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
      read FPointSize write SetPointSize {$ifdef FPC}default DefaultPointSize{$endif};

    { Line width. This has an effect on TLineSetNode rendering,
      and on wireframe rendering when @link(TCastleRenderOptions.WireframeEffect) indicates it.
      Must be > 0. }
    property LineWidth: Single
      read FLineWidth write SetLineWidth {$ifdef FPC}default DefaultLineWidth{$endif};

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
      read FBumpMappingParallaxHeight write SetBumpMappingParallaxHeight
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
      read FMaxLightsPerShape write SetMaxLightsPerShape default DefaultMaxLightsPerShape;

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

      See https://castle-engine.io/blending
      for more information about blending.

      For the exact meaning of BlendingSourceFactor and BlendingDestinationFactor
      consult OpenGL specification of glBlendFunc,
      https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml .
      Regardless of whether we use OpenGL for rendering, these properties
      and their values will reflect the equations there.

      The typical blending setup (default) is to use

      @unorderedList(
        @item(BlendingSourceFactor =
          DefaultBlendingSourceFactor =
          bsSrcAlpha.)
        @item(BlendingDestinationFactor =
          DefaultBlendingDestinationFactor =
          bdOneMinusSrcAlpha.
        )
      )

      This is the standard that follows most intuitive transparency equation.
      The major drawback of this default (and why you may want to consider
      alternatives) is that it depends on a correct order of rendering
      partially-transparent shapes.

      CGE allows to cope with it:

      @orderedList(
        @item(When multiple transparent shapes are possible,
          sort the transparent shapes, using
          @link(TCastleViewport.BlendingSort).

          Note that it cannot
          be perfect in certain cases (like when transparent shape is one, concave,
          mesh).
        )

        @item(For closed convex 3D objects, use backface culling.

          To do this, set on geometries @link(TAbstractGeometryNode.Solid)
          to @true, this activates backface culling.

          In practice you usually set this in a 3D authoring application,
          like Blender. This is called just "Backface Culling" in Blender,
          checkbox is in the Blender material properties.
          This information is then exported to glTF or X3D and just used
          by CGE.)

        @item(Finally, consider a different blending equation:

          Changing BlendingDestinationFactor to bdOne means that sorting
          isn't necessary. On the other hand, it only adds to the color,
          often making too bright results.
        )
      )

      Note that these properties can be overridden on each shape
      using @link(TBlendModeNode).
      See https://castle-engine.io/x3d_extensions.php#section_ext_blending
      for details of TBlendModeNode.

      @groupBegin }
    property BlendingSourceFactor: TBlendingSourceFactor
      read FBlendingSourceFactor write SetBlendingSourceFactor
      default DefaultBlendingSourceFactor;
    property BlendingDestinationFactor: TBlendingDestinationFactor
      read FBlendingDestinationFactor write SetBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;
    { @groupEnd }

    { Activate various effects related to wireframe rendering.
      When this is weNormal (default), we simply render polygons as polygons.
      See description of TWireframeEffect for what other modes do.

      Note: How the wireframe effects work when Mode = rmDepth is undefined now.
      Don't use Mode = rmDepth with WireframeEffect <> weNormal. }
    property WireframeEffect: TWireframeEffect
      read FWireframeEffect write SetWireframeEffect default weNormal;

    property SolidWireframeScale: Single
      read FSolidWireframeScale write SetSolidWireframeScale {$ifdef FPC}default DefaultSolidWireframeScale{$endif};
    property SolidWireframeBias: Single
      read FSolidWireframeBias write SetSolidWireframeBias {$ifdef FPC}default DefaultSolidWireframeBias{$endif};
    property SilhouetteScale: Single
      read FSilhouetteScale write SetSilhouetteScale {$ifdef FPC}default DefaultSilhouetteScale{$endif};
    property SilhouetteBias: Single
      read FSilhouetteBias write SetSilhouetteBias {$ifdef FPC}default DefaultSilhouetteBias{$endif};

    { Support lighting and backface culling for models using negative scale.
      This has a small performance cost, and so is disabled by default. }
    property RobustNegativeScale: Boolean
      read FRobustNegativeScale write SetRobustNegativeScale default false;

    { When considering this scene to be a shadow caster for shadow volumes,
      assume it is 2-manifold when treated as whole scene (each separate
      shape may be not 2-manifold). This means that you guarantee that
      all border edges of all shapes match with each other, closing the skin
      of the model perfectly.
      See https://castle-engine.io/shadow_volumes
      about using shadow volumes and the requirement to be 2-manifold.

      If this is @true then, even if some shapes are not 2-manifold,
      we will generate shadow (using shadow volumes) from them.
      And we will never reject shadows from a particular shape,
      we will either reject whole scene shadow, or render whole scene shadow.

      It is the user responsibility to guarantee that whole scene is
      really 2-manifold. If it is not, then shadow volumes may exhibit
      weird artifacts. }
    property WholeSceneManifold: Boolean
      read FWholeSceneManifold write SetWholeSceneManifold default false;

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
  FSolidWireframeScale := DefaultSolidWireframeScale;
  FSolidWireframeBias := DefaultSolidWireframeBias;
  FSilhouetteScale := DefaultSilhouetteScale;
  FSilhouetteBias := DefaultSilhouetteBias;
  FWireframeEffect := weNormal;
  FWireframeColor := DefaultWireframeColor;
  FColorChannels := AllColorChannels;

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
  end else
    inherited;
end;

function TCastleRenderOptions.EqualForShapeCache(
  SecondValue: TCastleRenderOptions): boolean;
begin
  Result :=
    (SecondValue.Textures = Textures);
end;

function TCastleRenderOptions.Equals(Obj: TObject): Boolean;
begin
  Result :=
    (Obj is TCastleRenderOptions) and
    (FHash = TCastleRenderOptions(Obj).FHash);
end;

procedure TCastleRenderOptions.ReleaseCachedResources;
begin
end;

{$I norqcheckbegin.inc}
procedure TCastleRenderOptions.UpdateHash;

  function SingleOrd(const S: Single): TRenderOptionsHash;
  begin
    Result := Round(S * 1000);
  end;

  function ColorOrd(const C: TCastleColorRGB): TRenderOptionsHash;
  begin
    Result :=
      Round(C.X * 1000) +
      Round(C.Y * 1000 * 1000) +
      Round(C.Z * 1000 * 1000 * 1000);
  end;

  (* Unused now.
  function PtrOrd(const P: Pointer): TRenderOptionsHash;
  begin
    { On 64-bit platforms, cut pointer to least significant 32 bits.
      That's OK for hash. }
    Result := TRenderOptionsHash(PtrUInt(P));
  end;
  *)

  function ColorChannelsOrd(const C: TColorChannels): TRenderOptionsHash;
  begin
    Result :=
      179 * Ord(0 in C) +
      181 * Ord(1 in C) +
      191 * Ord(2 in C) +
      193 * Ord(3 in C);
  end;

begin
  // https://en.wikipedia.org/wiki/List_of_prime_numbers
  FHash :=
    2 * Ord(FLighting) +
    3 * Ord(FReceiveSceneLights) +
    5 * Ord(FReceiveGlobalLights) +
    7 * Ord(FTextures) +
    11 * Ord(FMinificationFilter) +
    13 * Ord(FMagnificationFilter) +
    17 * SingleOrd(FPointSize) +
    19 * SingleOrd(FLineWidth) +
    23 * Ord(FBumpMapping) +
    29 * SingleOrd(FBumpMappingParallaxHeight) +
    41 * Ord(FMode) +
    43 * Ord(FShadowSampling) +
    47 * Ord(FDepthTest) +
    53 * Ord(FPhongShading) +
    59 * ColorOrd(FSolidColor) +
    73 * Ord(FMaxLightsPerShape) +
    79 * Ord(FBlending) +
    83 * Ord(FBlendingSourceFactor) +
    89 * Ord(FBlendingDestinationFactor) +
    97 * ColorOrd(FWireframeColor) +
    101 * Ord(FWireframeEffect) +
    103 * SingleOrd(FSolidWireframeScale) +
    107 * SingleOrd(FSolidWireframeBias) +
    109 * SingleOrd(FSilhouetteScale) +
    113 * SingleOrd(FSilhouetteBias) +
    127 * Ord(FRobustNegativeScale) +
    131 * Ord(FWholeSceneManifold) +
    137 * ColorChannelsOrd(FColorChannels);
end;
{$I norqcheckend.inc}

procedure TCastleRenderOptions.CustomSerialization(const SerializationProcess: TSerializationProcess);

  function SceneCaption: String;
  begin
    if Owner <> nil then
      Result := 'Scene "' + Owner.Name + '"'
    else
      Result := 'RenderOptions (in unnamed scene)';
  end;

var
  BlendingSortStr: String;
  B: Boolean;
begin
  inherited;
  BlendingSortStr := '';
  SerializationProcess.ReadWriteString('BlendingSort', BlendingSortStr, false);
  if BlendingSortStr <> '' then
  begin
    WritelnWarning('%s uses "BlendingSort" = "%s", this is no longer supported on scene. Instead set TCastleViewport.BlendingSort.', [
      SceneCaption,
      BlendingSortStr
    ]);
  end;

  B := false;
  SerializationProcess.ReadWriteBoolean('OcclusionSort', B, false);
  if B then
  begin
    WritelnWarning('%s uses "OcclusionSort", this is no longer supported on scene. Instead set TCastleViewport.OcclusionSort.', [
      SceneCaption
    ]);
  end;

  B := false;
  SerializationProcess.ReadWriteBoolean('OcclusionQuery', B, false);
  if B then
  begin
    WritelnWarning('%s uses "OcclusionQuery", this is no longer supported on scene. Instead set TCastleViewport.OcclusionCulling.', [
      SceneCaption
    ]);
  end;
end;

function TCastleRenderOptions.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
        'Lighting', 'ReceiveSceneLights', 'ReceiveGlobalLights', 'Textures',
        'MinificationFilter', 'MagnificationFilter', 'PointSize', 'LineWidth',
        'BumpMapping', 'BumpMappingParallaxHeight', 'PhongShading', 'ShadowSampling',
        'MaxLightsPerShape', 'Blending', 'BlendingSourceFactor', 'BlendingDestinationFactor',
        'WireframeEffect',
        'SolidWireframeScale', 'SolidWireframeBias', 'SilhouetteScale',
        'SilhouetteBias', 'RobustNegativeScale',
        'WholeSceneManifold', 'ColorChannels'
      ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ trivial setters ------------------------------------------------------------- }

procedure TCastleRenderOptions.SetTextures(const Value: Boolean);
begin
  if Textures <> Value then
  begin
    ReleaseCachedResources;
    FTextures := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetMinificationFilter(const Value: TAutoMinificationFilter);
begin
  if MinificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMinificationFilter := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetMagnificationFilter(const Value: TAutoMagnificationFilter);
begin
  if MagnificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMagnificationFilter := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetBumpMapping(const Value: TBumpMapping);
begin
  if BumpMapping <> Value then
  begin
    ReleaseCachedResources;
    FBumpMapping := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetMode(const Value: TRenderingMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    UpdateHash;
  end;
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
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetBlending(const Value: Boolean);
begin
  if FBlending <> Value then
  begin
    FBlending := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetBlendingSourceFactor(
  const Value: TBlendingSourceFactor);
begin
  if FBlendingSourceFactor <> Value then
  begin
    FBlendingSourceFactor := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetBlendingDestinationFactor(
  const Value: TBlendingDestinationFactor);
begin
  if FBlendingDestinationFactor <> Value then
  begin
    FBlendingDestinationFactor := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetPhongShading(const Value: Boolean);
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
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetWireframeColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FWireframeColor, Value) then
  begin
    FWireframeColor := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetWireframeEffect(const Value: TWireframeEffect);
begin
  if FWireframeEffect <> Value then
  begin
    FWireframeEffect := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetSolidWireframeScale(const Value: Single);
begin
  if FSolidWireframeScale <> Value then
  begin
    FSolidWireframeScale := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetSolidWireframeBias(const Value: Single);
begin
  if FSolidWireframeBias <> Value then
  begin
    FSolidWireframeBias := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetSilhouetteScale(const Value: Single);
begin
  if FSilhouetteScale <> Value then
  begin
    FSilhouetteScale := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetSilhouetteBias(const Value: Single);
begin
  if FSilhouetteBias <> Value then
  begin
    FSilhouetteBias := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetRobustNegativeScale(const Value: Boolean);
begin
  if FRobustNegativeScale <> Value then
  begin
    FRobustNegativeScale := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetWholeSceneManifold(const Value: Boolean);
begin
  if FWholeSceneManifold <> Value then
  begin
    FWholeSceneManifold := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetColorChannels(const Value: TColorChannels);
begin
  if FColorChannels <> Value then
  begin
    FColorChannels := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetMaxLightsPerShape(const Value: Cardinal);
begin
  if FMaxLightsPerShape <> Value then
  begin
    FMaxLightsPerShape := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetDepthTest(const Value: Boolean);
begin
  if FDepthTest <> Value then
  begin
    FDepthTest := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetReceiveSceneLights(const Value: Boolean);
begin
  if FReceiveSceneLights <> Value then
  begin
    FReceiveSceneLights := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetReceiveGlobalLights(const Value: Boolean);
begin
  if FReceiveGlobalLights <> Value then
  begin
    FReceiveGlobalLights := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetSolidColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FSolidColor, Value) then
  begin
    FSolidColor := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetPointSize(const Value: Single);
begin
  if FPointSize <> Value then
  begin
    FPointSize := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetLineWidth(const Value: Single);
begin
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetBumpMappingParallaxHeight(const Value: Single);
begin
  if FBumpMappingParallaxHeight <> Value then
  begin
    FBumpMappingParallaxHeight := Value;
    UpdateHash;
  end;
end;

procedure TCastleRenderOptions.SetLighting(const Value: Boolean);
begin
  if FLighting <> Value then
  begin
    FLighting := Value;
    UpdateHash;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlerenderoptions_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
