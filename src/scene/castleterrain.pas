{
  Copyright 2009-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Terrain (height map) components.

  TODO: Terrain API is not finalized yet,
  there may be significant incompatible changes to this unit in the future.
  Big TODOs:

  @unorderedList(
    @item(
      Edit terrain heights in the CGE editor.

      Possible approach: turn any data (including TCastleTerrainNoise) into
      TCastleTerrainImage, then add editing tools to just change this image.

      Alternatively user could edit a TCastleTerrainImage that is combined
      with TCastleTerrainNoise -- this seems a powerful but uncertain feature,
      i.e. in the end it means you cannot directly edit the output of TCastleTerrainNoise,
      so it may be less comfortable (although it would allow to later regenerate
      TCastleTerrainNoise and keep the edits -- effectively they would work
      like Blender modifier.)
    )

    @item(
      Edit textures influence using splatmap. Just a texture where RGBA
      are weights for each texture.

      Implement this, and also add a brush to edit splatmap in CGE editor.

      The existing way to determine layers influence can then be used
      as just a nice (optional) way to auto-generate initial splatmap.)

    @item(Adding vegetation to the terrain.

      Adding grass and other small things (that must be placed in large batches
      and density should be dynamic).

      Adding trees, boulders and other large things (their density should
      also be dynamic for fast rendering, but at close distance they are just
      3D models).)

    @item(
      Add non-trivial rendering algorithm.
      Right now it is really the simplest possible approach to generate a mesh
      from heights with a simplest shader mixing a few texture layers.)
  )

  Smaller TODOs:

  @unorderedList(
    @item(TCastleTerrain.Subdivisions should be TVector2Cardinal.
      Needs introducing TCastleVector2CardinalPersistent.)
    @item(Enable normalmaps for each layer.)
    @item(? TCastleTerrain configurable textures number.
      Unsure -- hardcoding current 4 layers sucks, but also it makes sense for coming RGBA splatmap.)
    @item(? TCastleTerrain Use GridCount, GridStep instead of Subdivisions, Size?
      See wyrd-forest arguments.)
    @item(? TCastleTerrain add property to define material type, like in primitives - unlit, physical, phong.
      Wait with this for our final CGE components to define materials and effects,
      TCastlePhysicalMaterial, TCastleShaderEffect -- they will determine how to nicely
      expose it here.)
  )
}
unit CastleTerrain;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleClassUtils, CastleScript, CastleImages, X3DNodes, CastleVectors,
  CastleRectangles, CastleTransform, CastleScene, X3DFields, CastleRenderOptions,
  CastleColors, CastleTriangles;

type
  { Terrain (height map) data that can be used for @link(TCastleTerrain.Data). }
  TCastleTerrainData = class(TCastleComponent)
  strict private
    FChangeNotifications: TNotifyEventList;
    procedure UpdateNodeCore(const Node: TAbstractChildNode;
      const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle;
      const Appearance: TAppearanceNode);
  private
    { Create X3D node with the given terrain shape.

      InputRange determines the range of values (minimum and maximum X and Y)
      queried from the underlying terrain, that is: queried using the @link(Height)
      method.

      OutputRange determines the span of the resulting shape in X and Z
      coordinates. Note that, when converting a heightmap to 3D,
      the 2nd dimension is Z. In 3D, we use Y for height, as this is the default
      in X3D and Castle Game Engine.

      Often you will set InputRange and OutputRange to the same values,
      but you don't have to. Often they will at least have the same aspect ratio
      (actually, often they will be square), but you don't have to.

      Divisions determines how dense is the mesh.

      We return the node that you should insert to your scene.

      It will use the given TAppearanceNode.
      The appearance is not configured in any way by this method,
      and it can even be @nil. }
    function CreateNode(const Subdivisions: TVector2;
      const InputRange, OutputRange: TFloatRectangle;
      const Appearance: TAppearanceNode): TAbstractChildNode; overload;

    { Update a node created by @link(CreateNode)
      to the new terrain and it's settings.
      The appearance of the previously created node (in CreateNode) is preserved. }
    procedure UpdateNode(const Node: TAbstractChildNode;
      const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle);

    { Alternative version of @link(CreateNode) that creates a different shape.
      It's has little less quality (triangulation is not adaptive like
      for ElevationGrid), but updating it (by UpdateTriangulatedNode)
      is a little faster (than updating the CreateNode by UpdatNode).
      In practice, the speed gain is minimal, and this method will likely
      be removed at some point.

      The parameters have the same meaning as for @link(CreateNode),
      and resulting look should be the same. }
    function CreateTriangulatedNode(const Subdivisions: TVector2;
      const InputRange, OutputRange: TFloatRectangle;
      const Appearance: TAppearanceNode): TAbstractChildNode;

    { Update a node created by @link(CreateTriangulatedNode)
      to the new terrain and it's settings. }
    procedure UpdateTriangulatedNode(const Node: TAbstractChildNode;
      const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle);
  protected
    { Use this in descendants implementation to notify that data
      (affecting @link(Height) results) changed.
      In the base class, it takes care to run notifications registered
      by @link(AddChangeNotification). }
    procedure DoChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Return height for given terrain point.

      @param(Coord The queried terrain height.
        Coord.X is the X position in local terrain coordinate space.
        Coord.Y is the Z position in local terrain coordinate space.
        They are in the range determined by TCastleTerrain.Size.
      )
      @param(TexCoord The texture coordinate at this point,
        assuming that TCastleTerrain.Size and texture UV scale would cancel each other,
        making both TexCoord.X and TexCoord.Y always in [0..1] range.

        Note that TexCoord.Y grows in different direction than Coord.Y,
        this means that textures have natural look when viewer from above the terrain
        (otherwise they would seem flipped in one direction in CGE right-handed
        coordinate system).
      ) }
    function Height(const Coord, TexCoord: TVector2): Single; virtual; abstract;

    { Add notification when data (affecting @link(Height) results) changes. }
    procedure AddChangeNotification(const Notify: TNotifyEvent);
    { Remove notification when data (affecting @link(Height) results) changes. }
    procedure RemoveChangeNotification(const Notify: TNotifyEvent);
  end;

  { Terrain (height map) data taken from intensities in an image.

    The image spans always the entire terrain in X and Z.

    The image minimum intensity (black) maps to height MinLevel,
    maximum intensity (white) maps to height MaxLevel.
    Any relation of MinLevel and MaxLevel is OK,
    that is: both MinLevel<MaxLevel and MinLevel>MaxLevel are valid.

    When image is not loaded, it behaves like all the image intensities are 0.5. }
  TCastleTerrainImage = class(TCastleTerrainData)
  strict private
    { FImage = nil and FUrl = '' when not loaded. }
    FImage: TGrayscaleImage;
    FUrl: String;
    FMinLevel, FMaxLevel: Single;
    procedure SetUrl(const Value: String);
    procedure SetMinLevel(const Value: Single);
    procedure SetMaxLevel(const Value: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Height(const Coord, TexCoord: TVector2): Single; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Image URL. Empty string means that no image is loaded. }
    property Url: String read FUrl write SetUrl;
    { Height when the image has minimum intensity (black). }
    property MinLevel: Single read FMinLevel write SetMinLevel {$ifdef FPC}default -0.5{$endif};
    { Height when the image has maximum intensity (white). }
    property MaxLevel: Single read FMaxLevel write SetMaxLevel {$ifdef FPC}default  0.5{$endif};
  end;

  { Terrain (height for each X, Y) data calculated from CastleScript
    expression. At construction, pass FunctionExpression,
    that is CastleScript language expression calculating height
    based on X, Y.

    See https://castle-engine.io/castle_script.php for CastleScript syntax.
    Try e.g. function like

    - @code(sin(x) + sin(y))

    - Sum sinusoides of various frequencies and amplitudes:

      @preformatted(
        (sin(x) + sin(x*2) / 2 + sin(x*4) / 4)  *
        (sin(y) + sin(y*2) / 2 + sin(y*4) / 4)
      ) }
  TTerrainCasScript = class(TCastleTerrainData)
  strict private
    FXVariable, FYVariable: TCasScriptFloat;
    FFunction: TCasScriptExpression;
  public
    constructor Create(const FunctionExpression: string); reintroduce;
    destructor Destroy; override;
    function Height(const Coord, TexCoord: TVector2): Single; override;
  end deprecated 'using CastleScript to define terrain is deprecated due to low usage';

  TNoiseInterpolation = (niNone, niLinear, niCosine, niSpline);

  { Terrain heights are generated from a smooth noise,
    combined with some terrain-specific improvements (Heterogeneous).

    We take the noise (integer noise, i.e. hash), smooth it
    (how well, and how fast --- see @link(Interpolation) and @link(Blur)),
    and add several
    functions ("octaves") of such noise (with varying frequency and amplitude)
    together. This is the kind of noise used to synthesize textures,
    terrains and all other procedural stuff.

    For more info about math inside:

    @unorderedList(
      @item([http://en.wikipedia.org/wiki/Fractional_Brownian_motion].
        This is the idea of summing up octaves of noise.
        Ken Musgrave's dissertation has a lot of info and interesting references:
        [http://www.kenmusgrave.com/dissertation.html])

      @item(Blender's source code is informative, interesting file
        is blender/source/blender/blenlib/intern/noise.c)

      @item(The simplest practical introduction to the idea is on
        [http://freespace.virgin.net/hugo.elias/models/m_perlin.htm].
        It describes how to get nice noise very easily, and my approach follows
        theirs.)
    ) }
  TCastleTerrainNoise = class(TCastleTerrainData)
  strict private
    type
      TNoise2DMethod = function (const X, Y: Single; const Seed: Cardinal): Single;
    var
      FOctaves: Single;
      FSmoothness: Single;
      FAmplitude: Single;
      FFrequency: Single;
      FInterpolation: TNoiseInterpolation;
      NoiseMethod: TNoise2DMethod;
      FBlur: Boolean;
      FSeed: Cardinal;
      FHeterogeneous: Single;
    procedure SetOctaves(const Value: Single);
    procedure SetSmoothness(const Value: Single);
    procedure SetAmplitude(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetInterpolation(const Value: TNoiseInterpolation);
    procedure SetBlur(const Value: Boolean);
    procedure SetSeed(const Value: Cardinal);
    procedure SetHeterogeneous(const Value: Single);
    procedure UpdateNoiseMethod;
  public
    const
      DefaultOctaves = 4.0;
      DefaultSmoothnes = 2.0;
      DefaultAmplitude = 8.0;
      DefaultFrequency = 0.05;
      DefaultInterpolation = niCosine;
      DefaultBlur = false;
      DefaultHeterogeneous = 0.5;

    constructor Create(AOwner: TComponent); override;
    function Height(const Coord, TexCoord: TVector2): Single; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Number of noise functions to sum.
      This linearly affects the time for Height call, so don't make
      it too much. Usually ~a few are Ok.

      (The fact that it's a float is just a simple trick to allow smooth
      transitions from x to x+1. In fact, it's executed like
      Trunc(Octaves) * some noises + Frac(Octaves) * some last noise.)

      Reasonable values are roughly between 0..20. }
    property Octaves: Single read FOctaves write SetOctaves {$ifdef FPC}default DefaultOctaves{$endif};

    { How noise amplitude changes, when frequency doubles.
      When we double frequency, amplitude is divided by this.
      Smaller values <=> larger frequency noise
      is more visible, so terrain is less smooth (more noisy).

      This is elsewhere called fractal increment, fractal dimension parameter,
      "H", spectral exponent (see e.g. Blender sources, Musgrave's dissertation).
      Do not confuse this with "lacunarity" (how frequency changes in each octave),
      that is simply hardcoded to 2.0 in our code currently.
      In [http://freespace.virgin.net/hugo.elias/models/m_perlin.htm],
      the inverse of this 1/Smoothness is called "Persistence".

      I decided to call it "Smoothness", since this is the practical
      intuitive meaning.

      Value equal 1.0 means that amplitude doesn't change at all,
      each noise frequency is visible the same, so in effect you will
      just see a lot of noise. And values < 1.0 are really nonsense,
      they make more frequency noise even more visible, which means that
      the terrain is dominated by noise.

      Reasonable values are roughly between 1..10. }
    property Smoothness: Single read FSmoothness write SetSmoothness {$ifdef FPC}default DefaultSmoothnes{$endif};

    { Amplitude and frequency of the first noise octave.
      Amplitude scales the height of the result, and Frequency scales
      the size of the bumps.
      @groupBegin }
    property Amplitude: Single read FAmplitude write SetAmplitude {$ifdef FPC}default DefaultAmplitude{$endif};
    property Frequency: Single read FFrequency write SetFrequency {$ifdef FPC}default DefaultFrequency{$endif};
    { @groupEnd }

    { How integer noise is interpolated to get smooth float noise.

      Setting this to niNone turns off interpolation, which means that
      your terrain is a sum of a couple of blocky noises --- ugly.

      Using niLinear (means "bilinear", since this is 2D case)
      is also usually bad. Unless you use octaves of really high frequencies,
      usually sharp edges  / flat in-betweens will be visible.

      Using niCosine in right now the best.

      Using niSpline is even better looking
      (usese Catmull-Rom splines,
      which are special case of cubic Hermite spline, see
      http://en.wikipedia.org/wiki/Cubic_Hermite_spline,
      http://en.wikipedia.org/wiki/Bicubic_interpolation).
      But it's more time consuming under current implementation. }
    property Interpolation: TNoiseInterpolation
      read FInterpolation write SetInterpolation default DefaultInterpolation;

    { Resulting noise octaves may be blurred. This helps to remove
      the inherent vertical/horizontal directionality in our 2D noise
      (it also makes it more smooth, since that's what blurring is about;
      you may want to increase Frequency * 2 to balance this).

      This is independent from @link(Interpolation). Although the need
      for Blur is most obvious in poor/none interpolation methods
      (none, linear), it also helps for the nicer interpolation methods
      (cosine, cubic).

      Note about [http://freespace.virgin.net/hugo.elias/models/m_perlin.htm]:
      this "blurring" is called "smoothing" there.
      I call it blurring, as it seems more precise to me. }
    property Blur: Boolean read FBlur write SetBlur default DefaultBlur;

    { Determines the random seeds used when generating the terrain. }
    property Seed: Cardinal read FSeed write SetSeed default 0;

    { If non-zero, then we generate terrain using heterogeneous fBm.
      Intuitively, the idea is that the terrain details (from higher octaves)
      are more noisy when ground is higher. This is realistic
      (debris gathers in lower terrain, smoothing it more).

      More precisely, this means that we accumulate multiplied previous noise,
      at each step dividing this accumulated result by Heterogeneous,
      and clamping at 1.0. So when Heterogeneous is very small,
      this always ends up 1.0, and we get normal (homogeneous) generation.
      When Heterogeneous is larger, the details (at lower ground)
      are scaled down (terrain is smoother).

      This is called "threshold" in Musgrave's dissertation (see algorithm
      in section 2.3.2.5 "A Large Scale Terrain Model").

      Reasonable values for this are between 0..2. }
    property Heterogeneous: Single
      read FHeterogeneous write SetHeterogeneous {$ifdef FPC}default DefaultHeterogeneous{$endif};
  end;

  TTerrain = TCastleTerrainData deprecated 'use TCastleTerrainData';
  TTerrainImage = TCastleTerrainImage deprecated 'use TCastleTerrainImage';
  TTerrainNoise = TCastleTerrainNoise deprecated 'use TCastleTerrainNoise';

  { Operation used by TCastleTerrainCombine to combine heights from 2 terrain data sources.
    See @link(TCastleTerrainCombine.Operation). }
  TCastleTerrainCombineOperation = (opMax, opMin, opAdd, opMultiply);

  { Combine (add, multiply, do maximum or minimum) two other terrain data sources.
    This allows to e.g. combine TCastleTerrainNoise with TCastleTerrainImage
    in a flexible way. For example you can use TCastleTerrainImage to force some
    mountains / valleys even where TCastleTerrainNoise doesn't have them. }
  TCastleTerrainCombine = class(TCastleTerrainData)
  private
    type
      TOperationFunc = function (const A, B: Single): Single;
    var
      FData1, FData2: TCastleTerrainData;
      FData1Observer, FData2Observer: TFreeNotificationObserver;
      FOperation: TCastleTerrainCombineOperation;
      FOperationFunc: TOperationFunc;
    procedure Data1FreeNotification(const Sender: TFreeNotificationObserver);
    procedure Data2FreeNotification(const Sender: TFreeNotificationObserver);
    procedure Data1Changed(Sender: TObject);
    procedure Data2Changed(Sender: TObject);
    procedure SetData1(const Value: TCastleTerrainData);
    procedure SetData2(const Value: TCastleTerrainData);
    procedure SetOperation(const Value: TCastleTerrainCombineOperation);
    procedure UpdateOperationFunc;
  public
    const
      DefaultOperation = opMax;
    constructor Create(AOwner: TComponent); override;
    function Height(const Coord, TexCoord: TVector2): Single; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { First data for terrain heights. }
    property Data1: TCastleTerrainData read FData1 write SetData1;
    { Second data for terrain heights. }
    property Data2: TCastleTerrainData read FData2 write SetData2;
    { How to combine results of Data1 and Data2. }
    property Operation: TCastleTerrainCombineOperation read FOperation write SetOperation default opMax;
  end;

  TCastleTerrain = class;

  { Layer of a terrain properties. See @link(TCastleTerrain) for docs how terrain
    uses layers for display. }
  TCastleTerrainLayer = class(TCastleComponent)
  strict private
    Terrain: TCastleTerrain;
    Layer: Integer;
    ColorField: TSFColor;
    TextureNode: TImageTextureNode;
    function GetUvScale: Single;
    procedure SetUvScale(const Value: Single);
    function GetMetallic: Single;
    procedure SetMetallic(const Value: Single);
    function GetRoughness: Single;
    procedure SetRoughness(const Value: Single);
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
    function GetTexture: String;
    procedure SetTexture(const Value: String);
    function GetUsingPackedVector(const Vec: TSFVec4f): Single;
    procedure SetUsingPackedVector(const Vec: TSFVec4f; const Value: Single);
  {$warnings off}
  private
    constructor CreateForTerrain(const ATerrain: TCastleTerrain;
      const ALayer: Integer; const Effect: TEffectNode);
  public
  {$warnings on}
    const
      DefaultUvScale = 1.0;
      DefaultMetallic = 1.0;
      DefaultRoughness = 1.0;

    { Public constructor for this class raises exception.
      Never create instances of this class directly.
      The instances of this class should only be created by implementation of TCastleTerrain,
      access them as TCastleTerrain subcomponents like @link(TCastleTerrain.Layer1). }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Color, multiplied by @link(Texture).
      Default is white. }
    property Color: TCastleColorRGB read GetColor write SetColor;

  published
    { Scale the @link(Texture).
      Setting UV scale to be equal to 1/@link(TCastleTerrain.Size)
      reliably makes the texture image size match the whole terrain. }
    property UvScale: Single read GetUvScale write SetUvScale {$ifdef FPC}default DefaultUvScale{$endif};

    { Texture URL. Texture is multiplied by @link(Color).
      Default (none) behaves as if it was a white texture for calculation. }
    property Texture: String read GetTexture write SetTexture;

    { The metalness of the material; values range from 0.0 (non-metal) to 1.0 (metal). }
    property Metallic: Single read GetMetallic write SetMetallic {$ifdef FPC}default DefaultMetallic{$endif};

    { The roughness of the material; values range from 0.0 (smooth) to 1.0 (rough). }
    property Roughness: Single read GetRoughness write SetRoughness {$ifdef FPC}default DefaultRoughness{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleterrainlayer_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Terrain.

    Assign @link(Data) to provide some non-trivial height map, you can use there:

    @unorderedList(
      @item(@link(TCastleTerrainNoise) to generate a height map using a dedicated
        algorithm for terrain generation, using smooth noise and special tricks
        to have smooth and heteregeneous terrain.)

      @item(@link(TCastleTerrainImage) to generate a height map from intensities
        of a simple 2D image.)

      @item(@link(TCastleTerrainCombine) to combine the above options in any expression
        (take minimum, maximum, sum, multiply).)
    )

    The terrain starts as a standard mesh with a @link(TPhysicalMaterialNode) material.
    We apply on it a special affect to mix 4 layers, where each layer has a separate
    color and texture.

    @unorderedList(
      @item(
        Each layer has a color (white by default), texture (none by default,
        that behaves like white) and UV scale.
        See @link(TCastleTerrainLayer).
        Each layer is a property like @link(Layer1), @link(Layer2),
        @link(Layer3), @link(Layer4).)

      @item(Only the RBG channels of textures matter, alpha is ignored.)

      @item(Layer 1 is used for flat terrain on lower heights.)

      @item(Layer 2 is used for steep terrain on lower heights.)

      @item(Layer 3 is used for flat terrain on higher heights.)

      @item(Layer 4 is used for steep terrain on higher heights.)

      @item(The meaning of "lower" and "higher" heights is determined by @link(Height1)
        and @link(Height2). Below @link(Height1) we show only layers 1+2,
        above @link(Height2) we show only layers 3+4,
        between we show a smooth interpolation between them.)

      @item(The meaning of "flat" and "steep" is determined by looking at terrain
        normals emphasized by @link(SteepEmphasize).)

      @item(The influence of this effect can be controlled by LayersInfluence.)
    )
  }
  TCastleTerrain = class(TCastleTransform)
  public
    const
      { Texture layers to render this terrain. }
      LayersCount = 4;
      HeightsCount = 2;
  strict private
    TerrainNode: TAbstractChildNode;
    Appearance: TAppearanceNode;
    Effect: TEffectNode;
    HeightsFields: array [1..HeightsCount] of TSFFloat;
    Layers: array [1..LayersCount] of TCastleTerrainLayer;
    FData: TCastleTerrainData;
    FDataObserver: TFreeNotificationObserver;
    FTriangulate: Boolean;
    FSize: TVector2;
    FLayersInfluence: Single;
    FSteepEmphasize: Single;
    FSubdivisions: TVector2;
    FPreciseCollisions: Boolean;
    FUpdateGeometryWhenLoaded: Boolean;
    function GetRenderOptions: TCastleRenderOptions;
    function GetLayer(const Index: Integer): TCastleTerrainLayer;
    procedure DataFreeNotification(const Sender: TFreeNotificationObserver);
    procedure DataChanged(Sender: TObject);

    { Regenerate geometry (vertexes, normals etc.) to show the current Data
      with current parameters. }
    procedure UpdateGeometry;

    procedure SetData(const Value: TCastleTerrainData);
    procedure SetTriangulate(const Value: Boolean);
    procedure SetSubdivisions(const Value: TVector2);
    procedure SetSize(const Value: TVector2);
    function GetHeight(const Index: Integer): Single;
    procedure SetHeight(const Index: Integer; const Value: Single);
    procedure SetLayersInfluence(const Value: Single);
    procedure SetSteepEmphasize(const Value: Single);
    procedure SetPreciseCollisions(const Value: Boolean);
  private
    Scene: TCastleScene;
    UvScaleField, MetallicField, RoughnessField: TSFVec4f;
  protected
    procedure Loaded; override;
  public
    const
      DefaultSubdivisions = 64;
      DefaultSize = 100;
      DefaultHeight1 = 4.0;
      DefaultHeight2 = 8.0;
      { Default values for @link(Height1), @link(Height2) etc.

        Note: This array duplicates information in constants
        @link(DefaultHeight1), @link(DefaultHeight2) etc.
        Unfortunately we need the simple constants too, to specify properties default values
        like "default DefaultHeight0". Using "default DefaultHeight[0]" doesn't work in FPC
        (nor in Delphi, but that's because Delphi cannot handle Single defaults at all). }
      DefaultHeight: array [1..HeightsCount] of Single = (
        DefaultHeight1,
        DefaultHeight2
      );
      DefaultLayersInfluence = 1.0;
      DefaultSteepEmphasize = 2.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function HasColliderMesh: Boolean; override;
    procedure ColliderMesh(const TriangleEvent: TTriangleEvent); override;

    { How dense is the mesh.
      By default this is (DefaultSubdivisions,DefaultSubdivisions).

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Subdivisions: TVector2 read FSubdivisions write SetSubdivisions;

    { Size of the generated shape and also the underlying range to query
      the TCastleTerrainNoise data for heights.

      Note that changing this does not just scale the same geometry,
      if TCastleTerrainNoise is used for @link(Data).
      The TCastleTerrainNoise uses the size you set here to determine
      what heights to query from a smooth noise.
      This has a nice effect that increasing the size adds additional
      pieces of terrain adjacent to the previous terrain,
      and the previous terrain shape is still visible at the same place.

      Be sure to increase also @link(Subdivisions) when increasing
      this field, to keep seeing the same detail.

      By default this is (DefaultSize,DefaultSize).

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Size: TVector2 read FSize write SetSize;
  published
    { Options used to render the terrain. Can be used e.g. to toggle wireframe rendering. }
    property RenderOptions: TCastleRenderOptions read GetRenderOptions;

    { 1st layer is displayed on lower heights and more flat terrain.
      See TCastleTerrain for a full description how do we mix layers. }
    property Layer1: TCastleTerrainLayer index 1 read GetLayer;

    { 2nd layer is displayed on lower heights and more steep terrain.
      See TCastleTerrain for a full description how do we mix layers. }
    property Layer2: TCastleTerrainLayer index 2 read GetLayer;

    { 3rd layer is displayed on higher heights and more flat terrain.
      See TCastleTerrain for a full description how do we mix layers. }
    property Layer3: TCastleTerrainLayer index 3 read GetLayer;

    { 4th layer is displayed on higher heights and more steep terrain.
      See TCastleTerrain for a full description how do we mix layers. }
    property Layer4: TCastleTerrainLayer index 4 read GetLayer;

    { Data for terrain heights.
      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Data: TCastleTerrainData read FData write SetData;

    { Do we generate as a set of triangles, or ElevationGrid.
      TODO: this is internal decision, should behave always like true?

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Triangulate: Boolean read FTriangulate write SetTriangulate default true;

    { How much should we emphasize the "steep" layers (2nd and 4th layers)
      at the expense of "flat" layers (1st and 3rd).

      SteepEmphasize = 0.0 means that only "flat" layers (1 and 3) are visible.
      SteepEmphasize = 1.0 means that flat vs steep is a linear interpolation
      based on normal Y coordinate (but, since most terrains are more flat than
      steep, in practice the "flat" layers are still more visible).
      As SteepEmphasize goes toward infinity, the "steep" layers (2 and 4) dominate. }
    property SteepEmphasize: Single read FSteepEmphasize write SetSteepEmphasize
      {$ifdef FPC}default DefaultSteepEmphasize{$endif};

    { Below Height1 we only display layers 1+2.
      See TCastleTerrain for a description when do we show show layers and how this property affects it. }
    property Height1: Single index 1 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight1{$endif};
    { Above Height2 we only display layers 3+4.
      See TCastleTerrain for a description when do we show show layers and how this property affects it. }
    property Height2: Single index 2 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight2{$endif};

    { How much do the layers affect the final color.
      0.0 means that layes are ignored, and the terrain look is a regular
      mesh look with @link(TPhysicalMaterialNode).
      1.0 means maximum influence, the layers determine the base color
      (TODO: and normals in the future). }
    property LayersInfluence: Single read FLayersInfluence write SetLayersInfluence
      {$ifdef FPC}default DefaultLayersInfluence{$endif};

    { Resolve collisions precisely with the terrain geometry.
      When this is @false we will only consider the terrain bounding box for collisions,
      which prevents moving on terrain nicely, picking terrain points with mouse etc.
      This sets @link(TCastleSceneCore.Spatial). }
    property PreciseCollisions: Boolean read FPreciseCollisions write SetPreciseCollisions default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleterrain_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

implementation

uses Math,
  CastleUtils, CastleScriptParser, CastleInternalNoise, CastleDownload, CastleLog,
  CastleURIUtils, CastleComponentSerialize;

{ TCastleTerrainData ------------------------------------------------------------------- }

constructor TCastleTerrainData.Create(AOwner: TComponent);
begin
  inherited;
  FChangeNotifications := TNotifyEventList.Create;
end;

destructor TCastleTerrainData.Destroy;
begin
  FreeAndNil(FChangeNotifications);
  inherited;
end;

procedure TCastleTerrainData.DoChange;
begin
  { TODO: To fully avoid useless UpdateGeometry calls,
    avoid calling FChangeNotifications when IsLoading.
    Only schedule it and call when Loaded. }

  FChangeNotifications.ExecuteAll(Self);
end;

procedure TCastleTerrainData.AddChangeNotification(const Notify: TNotifyEvent);
begin
  FChangeNotifications.Add(Notify);
end;

procedure TCastleTerrainData.RemoveChangeNotification(const Notify: TNotifyEvent);
begin
  if FChangeNotifications = nil then
  begin
    { This can happen when we free the TCastleTerrainData (like TCastleTerrainNoise)
      that is referenced in some TCastleTerrain.Data.
      Then TCastleTerrainData.Destroy does "FreeAndNil(FChangeNotifications)"
      first, then calls inherited destructor, that sends DataFreeNotification
      to TCastleTerrain which in turn in SetData does RemoveChangeNotification.
      So this all can happen in valid situation and can be just ignored.
      When FChangeNotifications = nil it means we're freeing ourselves
      and we no longer care about tracking FChangeNotifications. }
    Assert(csDestroying in ComponentState);
    Exit;
  end;
  FChangeNotifications.Remove(Notify);
end;

function TCastleTerrainData.CreateNode(const Subdivisions: TVector2;
  const InputRange, OutputRange: TFloatRectangle;
  const Appearance: TAppearanceNode): TAbstractChildNode;
begin
  Result := TTransformNode.Create;
  UpdateNodeCore(Result, Subdivisions, InputRange, OutputRange, Appearance);
end;

procedure TCastleTerrainData.UpdateNode(const Node: TAbstractChildNode;
  const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle);
var
  Transform: TTransformNode;
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
begin
  { extract Appearance from Node, assuming Node was created by CreateNode }
  Transform := Node as TTransformNode;
  Shape := Transform.FdChildren[0] as TShapeNode;
  Appearance := Shape.Appearance;

  Appearance.KeepExistingBegin;
  UpdateNodeCore(Node, Subdivisions, InputRange, OutputRange, Appearance);
  Appearance.KeepExistingEnd;
end;

procedure TCastleTerrainData.UpdateNodeCore(const Node: TAbstractChildNode;
  const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle;
  const Appearance: TAppearanceNode);
var
  Transform: TTransformNode;
  Shape: TShapeNode;
  Grid: TElevationGridNode;
  X, Z, SubdivisionsX, SubdivisionsZ: Cardinal;
  Coord, TexCoord: TVector2;
begin
  SubdivisionsX := Round(Subdivisions.X);
  SubdivisionsZ := Round(Subdivisions.Y);

  Transform := Node as TTransformNode; // created by CreateNode
  Transform.ClearChildren;
  Transform.Translation := Vector3(OutputRange.Left, 0, OutputRange.Bottom);

  Shape := TShapeNode.Create;

  Grid := TElevationGridNode.Create;
  Shape.FdGeometry.Value := Grid;
  Grid.FdCreaseAngle.Value := 4; { > pi, to be perfectly smooth }
  Grid.FdXDimension.Value := SubdivisionsX;
  Grid.FdZDimension.Value := SubdivisionsZ;
  Grid.FdXSpacing.Value := OutputRange.Width  / (SubdivisionsX - 1);
  Grid.FdZSpacing.Value := OutputRange.Height / (SubdivisionsZ - 1);
  Grid.FdHeight.Items.Count := SubdivisionsX * SubdivisionsZ;

  for X := 0 to SubdivisionsX - 1 do
    for Z := 0 to SubdivisionsZ - 1 do
    begin
      TexCoord := Vector2(
        MapRangeTo01(X, 0, SubdivisionsX - 1),
        MapRangeTo01(Z, 0, SubdivisionsZ - 1)
      );
      Coord := Vector2(
        Lerp(TexCoord.X, InputRange.Left  , InputRange.Right),
        Lerp(TexCoord.Y, InputRange.Bottom, InputRange.Top)
      );
      // TexCoord.Y grows in different direction, see Height docs for reason
      TexCoord.Y := 1 - TexCoord.Y;
      Grid.FdHeight.Items.List^[X + Z * SubdivisionsX] := Height(Coord, TexCoord);
    end;

  Shape.Appearance := Appearance;

  // at the end, as this may cause Scene.ChangedAll
  Transform.AddChildren(Shape);
end;

function TCastleTerrainData.CreateTriangulatedNode(const Subdivisions: TVector2;
  const InputRange, OutputRange: TFloatRectangle;
  const Appearance: TAppearanceNode): TAbstractChildNode;
var
  Geometry: TIndexedTriangleStripSetNode;
  CoordNode: TCoordinateNode;
  NormalNode: TNormalNode;
  Shape: TShapeNode;
  Transform: TTransformNode;
begin
  Geometry := TIndexedTriangleStripSetNode.Create;

  CoordNode := TCoordinateNode.Create;
  Geometry.Coord := CoordNode;

  NormalNode := TNormalNode.Create;
  Geometry.Normal := NormalNode;

  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  Transform := TTransformNode.Create;
  Transform.AddChildren(Shape);

  Result := Transform;

  UpdateTriangulatedNode(Result, Subdivisions, InputRange, OutputRange);
end;

procedure TCastleTerrainData.UpdateTriangulatedNode(const Node: TAbstractChildNode;
  const Subdivisions: TVector2; const InputRange, OutputRange: TFloatRectangle);
var
  SubdivisionsPlus1: TVector2Cardinal;
  Coord, Normal: TVector3List;
  Index: TInt32List;
  FaceNormals: TVector3List;
  SubdivisionsX, SubdivisionsZ: Cardinal;

  procedure CalculatePosition(const I, J: Cardinal; out Position: TVector3);
  var
    TexCoord, Coord: TVector2;
  begin
    TexCoord.X := I / (SubdivisionsX - 1);
    TexCoord.Y := J / (SubdivisionsZ - 1);
    Coord.X := InputRange.Width  * TexCoord.X + InputRange.Left;
    Coord.Y := InputRange.Height * TexCoord.Y + InputRange.Bottom;

    { Note that we don't shift by OutputRange.Left/Bottom by addition here,
      but by TTransformNode. This way in shader terrain_position (that determines
      UV for textures) starts from (0,0) at the corner (not middle) of the terrain,
      which makes it easier (and consistent with UpdateNode for ElevationGrid)
      to apply a texture that covers exactly the terrain once (matching
      TCastleTextureImage image mapping). }
    Position.X := OutputRange.Width  * TexCoord.X{ + OutputRange.Left};
    Position.Z := OutputRange.Height * TexCoord.Y{ + OutputRange.Bottom};

    // TexCoord.Y grows in different direction, see Height docs for reason
    TexCoord.Y := 1 - TexCoord.Y;

    Position.Y := Height(Coord, TexCoord);
  end;

  function Idx(const I, J: Integer): Integer;
  begin
    Result := I + J * SubdivisionsPlus1.X;
  end;

  procedure CalculateFaceNormal(const I, J: Cardinal; out Normal: TVector3);
  var
    P, PX, PY: PVector3;
  begin
    P  := PVector3(Coord.Ptr(Idx(I, J)));
    PX := PVector3(Coord.Ptr(Idx(I + 1, J)));
    PY := PVector3(Coord.Ptr(Idx(I, J + 1)));
    Normal := TVector3.CrossProduct(
      (PY^ - P^),
      (PX^ - P^)).Normalize;
  end;

  procedure CalculateNormal(const I, J: Cardinal; out Normal: TVector3);

    function FaceNormal(const DeltaX, DeltaY: Integer): TVector3;
    begin
      Result := FaceNormals.List^[Idx(I + DeltaX, J + DeltaY)];
    end;

  begin
    Normal := FaceNormal(0, 0);
    if (I > 0) then
      Normal := Normal + FaceNormal(-1, 0);
    if (J > 0) then
      Normal := Normal + FaceNormal(0, -1);
    if (I > 0) and (J > 0) then
      Normal := Normal + FaceNormal(-1, -1);
    Normal := Normal.Normalize;
  end;

var
  Transform: TTransformNode;
  Shape: TShapeNode;
  Geometry: TIndexedTriangleStripSetNode;
  CoordNode: TCoordinateNode;
  NormalNode: TNormalNode;
  I, J: Cardinal;
  IndexPtr: PInt32;
begin
  { extract nodes from Node, assuming it was created by CreateTriangulatedNode }
  Transform := Node as TTransformNode;
  Shape := Transform.FdChildren[0] as TShapeNode;
  Geometry := Shape.Geometry as TIndexedTriangleStripSetNode;
  CoordNode := Geometry.Coord as TCoordinateNode;
  NormalNode := Geometry.Normal as TNormalNode;

  Transform.Translation := Vector3(OutputRange.Left, 0, OutputRange.Bottom);

  SubdivisionsX := Round(Subdivisions.X);
  SubdivisionsZ := Round(Subdivisions.Y);

  { We render SubdivisionsX-1 squares (edges) along the X way,
    with SubdivisionsX points along the way.
    But calculate positions for SubdivisionsX + 1 points,
    because we need + 1 additional for normal calculation in CalculateFaceNormal. }
  SubdivisionsPlus1 := Vector2Cardinal(SubdivisionsX + 1, SubdivisionsZ + 1);

  Index := Geometry.FdIndex.Items;
  Coord := CoordNode.FdPoint.Items;
  Normal := NormalNode.FdVector.Items;

  { We will render (SubdivisionsX * SubdivisionsZ) points, but we want to calculate
    ((SubdivisionsX+1) * (SubdivisionsZ+1)) points:
    to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  Coord.Count := SubdivisionsPlus1.X * SubdivisionsPlus1.Y;
  Normal.Count := SubdivisionsPlus1.X * SubdivisionsPlus1.Y;

  { calculate Coord }
  for I := 0 to SubdivisionsX do
    for J := 0 to SubdivisionsZ do
      CalculatePosition(I, J, Coord.List^[Idx(I, J)]);
  CoordNode.FdPoint.Changed;

  { calculate Normals }
  FaceNormals := TVector3List.Create;
  try
    FaceNormals.Count := SubdivisionsPlus1.X * SubdivisionsPlus1.Y;
    { calculate per-face (flat) normals }
    for I := 0 to SubdivisionsX - 1 do
      for J := 0 to SubdivisionsZ - 1 do
        CalculateFaceNormal(I, J, FaceNormals.List^[Idx(I, J)]);
    { calculate smooth vertex normals }
    for I := 0 to SubdivisionsX - 1 do
      for J := 0 to SubdivisionsZ - 1 do
        CalculateNormal(I, J, Normal.List^[Idx(I, J)]);
  finally FreeAndNil(FaceNormals) end;
  NormalNode.FdVector.Changed;

  { calculate Index }
  Index.Count := (SubdivisionsX - 1) * (SubdivisionsZ * 2 + 1);
  IndexPtr := PInt32(Index.List);
  for I := 1 to SubdivisionsX - 1 do
  begin
    for J := 0 to SubdivisionsZ - 1 do
    begin
      // order to make it CCW when viewed from above
      IndexPtr^ := Idx(I    , J); Inc(IndexPtr);
      IndexPtr^ := Idx(I - 1, J); Inc(IndexPtr);
    end;
    IndexPtr^ := -1;
    Inc(IndexPtr);
  end;
  // make sure our Index.Count was set exactly to what we needed
  Assert((PtrUInt(IndexPtr) - PtrUInt(Index.List)) div SizeOf(Int32) = Index.Count);
  Geometry.FdIndex.Changed;
end;

{ TCastleTerrainImage ------------------------------------------------------------ }

constructor TCastleTerrainImage.Create(AOwner: TComponent);
begin
  inherited;
  FMinLevel := -0.5;
  FMaxLevel := 0.5;
end;

destructor TCastleTerrainImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TCastleTerrainImage.SetUrl(const Value: string);

  procedure LoadImage(const NewUrl: string);
  var
    NewImage: TGrayscaleImage;
  begin
    if NewUrl = '' then
      NewImage := nil
    else
      NewImage := CastleImages.LoadImage(NewUrl, [TGrayscaleImage]) as TGrayscaleImage;

    FreeAndNil(FImage);
    FImage := NewImage;
    FUrl := NewUrl;
    DoChange;
  end;

begin
  if FUrl <> Value then
  begin
    try
      LoadImage(Value); // in case of exception when loading, LoadImage leaves state unchanged
    except
      { If loading file failed, and we're inside CGE editor, merely report a warning.
        This allows deserializing in CGE editor designs with broken URLs. }
      on E: Exception do
      begin
        if CastleDesignMode then
        begin
          WritelnWarning('TCastleTerrainImage', 'Failed to load image "%s": %s', [
            URIDisplay(Value),
            ExceptMessage(E)
          ]);
        end else
          raise;
      end;
    end;
  end;
end;

function TCastleTerrainImage.Height(const Coord, TexCoord: TVector2): Single;
var
  PX, PY: Integer;
  Intensity: Single;
begin
  if FImage <> nil then
  begin
    PX := Floor(TexCoord.X * FImage.Width );
    PY := Floor(TexCoord.Y * FImage.Height);
    ClampVar(PX, 0, FImage.Width  - 1);
    ClampVar(PY, 0, FImage.Height - 1);
    Intensity := MapRangeTo01(FImage.PixelPtr(PX, PY)^, 0, High(Byte));
  end else
    Intensity := 0.5;
  Result := Lerp(Intensity, MinLevel, MaxLevel);
end;

procedure TCastleTerrainImage.SetMinLevel(const Value: Single);
begin
  if FMinLevel <> Value then
  begin
    FMinLevel := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainImage.SetMaxLevel(const Value: Single);
begin
  if FMaxLevel <> Value then
  begin
    FMaxLevel := Value;
    DoChange;
  end;
end;

function TCastleTerrainImage.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Url', 'MinLevel', 'MaxLevel'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TTerrainCasScript -------------------------------------------------------- }

constructor TTerrainCasScript.Create(const FunctionExpression: string);
begin
  inherited Create(nil);

  FXVariable := TCasScriptFloat.Create(false);
  FXVariable.Name := 'x';
  FXVariable.OwnedByParentExpression := false;

  FYVariable := TCasScriptFloat.Create(false);
  FYVariable.Name := 'y';
  FYVariable.OwnedByParentExpression := false;

  FFunction := ParseFloatExpression(FunctionExpression, [FXVariable, FYVariable]);
end;

destructor TTerrainCasScript.Destroy;
begin
  FFunction.FreeByParentExpression;
  FFunction := nil;

  FreeAndNil(FXVariable);
  FreeAndNil(FYVariable);

  inherited;
end;

function TTerrainCasScript.Height(const Coord, TexCoord: TVector2): Single;
begin
  FXVariable.Value := Coord.X;
  FYVariable.Value := Coord.Y;
  Result := (FFunction.Execute as TCasScriptFloat).Value;
end;

{ TCastleTerrainNoise ------------------------------------------------------------ }

constructor TCastleTerrainNoise.Create(AOwner: TComponent);
begin
  inherited;

  FOctaves := DefaultOctaves;
  FSmoothness := DefaultSmoothnes;
  FAmplitude := DefaultAmplitude;
  FFrequency := DefaultFrequency;
  FInterpolation := DefaultInterpolation;
  FBlur := DefaultBlur;
  FHeterogeneous := DefaultHeterogeneous;

  UpdateNoiseMethod;
end;

procedure TCastleTerrainNoise.UpdateNoiseMethod;
begin
  if Blur then
    case Interpolation of
      niNone: NoiseMethod := @BlurredInterpolatedNoise2D_None;
      niLinear: NoiseMethod := @BlurredInterpolatedNoise2D_Linear;
      niCosine: NoiseMethod := @BlurredInterpolatedNoise2D_Cosine;
      niSpline: NoiseMethod := @BlurredInterpolatedNoise2D_Spline;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTerrainNoise.UpdateNoiseMethod(Interpolation?)');
      {$endif}
    end else
    case Interpolation of
      niNone: NoiseMethod := @InterpolatedNoise2D_None;
      niLinear: NoiseMethod := @InterpolatedNoise2D_Linear;
      niCosine: NoiseMethod := @InterpolatedNoise2D_Cosine;
      niSpline: NoiseMethod := @InterpolatedNoise2D_Spline;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTerrainNoise.UpdateNoiseMethod(Interpolation?)');
      {$endif}
    end;
end;

procedure TCastleTerrainNoise.SetOctaves(const Value: Single);
begin
  if FOctaves <> Value then
  begin
    FOctaves := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetSmoothness(const Value: Single);
begin
  if FSmoothness <> Value then
  begin
    FSmoothness := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetAmplitude(const Value: Single);
begin
  if FAmplitude <> Value then
  begin
    FAmplitude := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetInterpolation(const Value: TNoiseInterpolation);
begin
  if FInterpolation <> Value then
  begin
    FInterpolation := Value;
    UpdateNoiseMethod;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetBlur(const Value: Boolean);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    UpdateNoiseMethod;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetSeed(const Value: Cardinal);
begin
  if FSeed <> Value then
  begin
    FSeed := Value;
    DoChange;
  end;
end;

procedure TCastleTerrainNoise.SetHeterogeneous(const Value: Single);
begin
  if FHeterogeneous <> Value then
  begin
    FHeterogeneous := Value;
    DoChange;
  end;
end;

function TCastleTerrainNoise.Height(const Coord, TexCoord: TVector2): Single;
// const
//   { Idea, maybe useful --- apply heterogeneous only on higher octaves.
//     Note that 1st octave is anyway always without heterogeneous,
//     so this is really useful only if setting to >= 2. }
//   HomogeneousOctaves = 2;
var
  A, F, NoiseAccumulator: Single;

  function NextOctave(const OctaveNumber: Cardinal): Single;
  begin
    { An explicit check for "Heterogeneous = 0" case is needed.

      Otherwise, when Heterogeneous = 0, "NoiseAccumulator /= Heterogeneous"
      calculates "0 / 0", which will not get us what we want.
      (What we want is to have NoiseAccumulator=+infinity,
      so that it gets clamped to 1, but it seems FPC 2.2.4 doesn't do this.
      FPC 2.4.0 seems to land on +infinity more often,
      but not when using Spline interpolation... Looks like "0 / 0"
      is simply undefined, and unsafe to use.)

      Note there's no need to check for IsZero(Heterogeneous).
      When Heterogeneous is close to zero, but not exactly zero,
      the +infinity trick will make the later code behave Ok. }
    if Heterogeneous = 0 then
      Exit(NoiseMethod(
        Coord.X * F,
        Coord.Y * F, OctaveNumber + Seed) * A);

    NoiseAccumulator := NoiseAccumulator / Heterogeneous;
    { Following Musgrave's dissertation, we should now force
      NoiseAccumulator to <0, 1> range.
      We know our NoiseMethod is always positive, and we require
      Amplitude, Heterogeneous and such to also be always positive.
      So we already know NoiseAccumulator is always >= 0. }
    MinVar(NoiseAccumulator, 1);

    NoiseAccumulator := NoiseAccumulator * NoiseMethod(
      Coord.X * F,
      Coord.Y * F, OctaveNumber + Seed);

    Result := NoiseAccumulator * A;
  end;

var
  I: Cardinal;
begin
  Result := 0;

  A := Amplitude;
  F := Frequency;
  { This will accumulate multiplication of noise octaves.
    Initial value is chosen so that at first step (I = 1)
    NoiseAccumulator will become 1.0, and then NoiseMethod() * A. }
  NoiseAccumulator := Heterogeneous;
  for I := 1 to Trunc(Octaves) do
  begin
    Result := Result + NextOctave(I);
    F := F * 2;
    A := A / Smoothness;
  end;

  { Add last octave's remainder.
    Just like a normal octave, but multiply by Frac(Octaves). }
  Result := Result + Frac(Octaves) * NextOctave(Trunc(Octaves) + 1);
end;

function TCastleTerrainNoise.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Octaves', 'Smoothness', 'Amplitude', 'Frequency', 'Interpolation', 'Blur', 'Seed', 'Heterogeneous'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleTerrainCombine ------------------------------------------------------ }

function CombineOperationMax(const A, B: Single): Single;
begin
  Result := Max(A, B);
end;

function CombineOperationMin(const A, B: Single): Single;
begin
  Result := Min(A, B);
end;

function CombineOperationAdd(const A, B: Single): Single;
begin
  Result := A + B;
end;

function CombineOperationMultiply(const A, B: Single): Single;
begin
  Result := A * B;
end;

constructor TCastleTerrainCombine.Create(AOwner: TComponent);
begin
  inherited;
  FData1Observer := TFreeNotificationObserver.Create(Self);
  FData1Observer.OnFreeNotification := {$ifdef FPC}@{$endif} Data1FreeNotification;
  FData2Observer := TFreeNotificationObserver.Create(Self);
  FData2Observer.OnFreeNotification := {$ifdef FPC}@{$endif} Data2FreeNotification;
  FOperation := DefaultOperation;
  UpdateOperationFunc;
end;

procedure TCastleTerrainCombine.UpdateOperationFunc;
begin
  case FOperation of
    opMax: FOperationFunc := {$ifdef FPC}@{$endif} CombineOperationMax;
    opMin: FOperationFunc := {$ifdef FPC}@{$endif} CombineOperationMin;
    opAdd: FOperationFunc := {$ifdef FPC}@{$endif} CombineOperationAdd;
    opMultiply: FOperationFunc := {$ifdef FPC}@{$endif} CombineOperationMultiply;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleTerrainCombine.UpdateOperationFunc?');
    {$endif}
  end;
end;

procedure TCastleTerrainCombine.SetOperation(const Value: TCastleTerrainCombineOperation);
begin
  if FOperation <> Value then
  begin
    FOperation := Value;
    UpdateOperationFunc;
    DoChange;
  end;
end;

function TCastleTerrainCombine.Height(const Coord, TexCoord: TVector2): Single;
begin
  if (Data1 <> nil) and (Data2 <> nil) then
  begin
    Result := FOperationFunc(
      Data1.Height(Coord, TexCoord),
      Data2.Height(Coord, TexCoord)
    );
  end else
  if Data1 <> nil then
    Result := Data1.Height(Coord, TexCoord)
  else
  if Data2 <> nil then
    Result := Data2.Height(Coord, TexCoord)
  else
    Result := 0;
end;

function TCastleTerrainCombine.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Data1', 'Data2', 'Operation'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleTerrainCombine.Data1FreeNotification(const Sender: TFreeNotificationObserver);
begin
  Data1 := nil;
end;

procedure TCastleTerrainCombine.Data2FreeNotification(const Sender: TFreeNotificationObserver);
begin
  Data2 := nil;
end;

procedure TCastleTerrainCombine.Data1Changed(Sender: TObject);
begin
  DoChange;
end;

procedure TCastleTerrainCombine.Data2Changed(Sender: TObject);
begin
  DoChange;
end;

function ContainsData(const Tree, ValueToFind: TCastleTerrainData): Boolean;
begin
  Result :=
    (Tree = ValueToFind) or
    (
      (Tree is TCastleTerrainCombine) and (
        ContainsData(TCastleTerrainCombine(Tree).Data1, ValueToFind) or
        ContainsData(TCastleTerrainCombine(Tree).Data2, ValueToFind)
      )
    );
end;

procedure TCastleTerrainCombine.SetData1(const Value: TCastleTerrainData);
begin
  if FData1 <> Value then
  begin
    { Ignore nonsense assignment done by FpRttiJson when this node references
      in JSON non-yet-known name. See TCastleTerrain.SetData comments. }
    if (Value <> nil) and
       (Value.ClassType = TCastleTerrainData) then
      Exit;

    // Protect from recursive usage hanging the process (editor or runtime)
    if (Value <> nil) and
       ContainsData(Value, Self) then
    begin
      raise Exception.CreateFmt('Setting %s as data for %s would make a recursive dependency (infinite calculation) in TCastleTerrainCombine', [
        Value.Name,
        Name
      ]);
      Exit;
    end;

    if FData1 <> nil then
      FData1.RemoveChangeNotification({$ifdef FPC}@{$endif} Data1Changed);
    FData1 := Value;
    FData1Observer.Observed := Value;
    if FData1 <> nil then
      FData1.AddChangeNotification({$ifdef FPC}@{$endif} Data1Changed);
    DoChange;
  end;
end;

procedure TCastleTerrainCombine.SetData2(const Value: TCastleTerrainData);
begin
  if FData2 <> Value then
  begin
    { Ignore nonsense assignment done by FpRttiJson when this node references
      in JSON non-yet-known name. See TCastleTerrain.SetData comments. }
    if (Value <> nil) and
       (Value.ClassType = TCastleTerrainData) then
      Exit;

    // Protect from recursive usage hanging the process (editor or runtime)
    if (Value <> nil) and
       ContainsData(Value, Self) then
    begin
      raise Exception.CreateFmt('Setting %s and data for %s would make a recursive dependency (infinite calculation) in TCastleTerrainCombine', [
        Value.Name,
        Name
      ]);
      Exit;
    end;

    if FData2 <> nil then
      FData2.RemoveChangeNotification({$ifdef FPC}@{$endif} Data2Changed);
    FData2 := Value;
    FData2Observer.Observed := Value;
    if FData2 <> nil then
      FData2.AddChangeNotification({$ifdef FPC}@{$endif} Data2Changed);
    DoChange;
  end;
end;

{ TCastleTerrainLayer -------------------------------------------------------- }

const
  { URL of a white pixel texture, embedded PPM.
    See http://netpbm.sourceforge.net/doc/ppm.html . }
  WhitePixel = 'data:image/x-portable-pixmap,P3'#10 +
    '1 1'#10 +
    '255'#10 +
    '255 255 255';

constructor TCastleTerrainLayer.Create(AOwner: TComponent);
begin
  inherited;
  raise Exception.Create('TCastleTerrainLayer can only be instantiated by TCastleTerrain as a subcomponent');
end;

constructor TCastleTerrainLayer.CreateForTerrain(const ATerrain: TCastleTerrain;
  const ALayer: Integer; const Effect: TEffectNode);
var
  TextureField: TSFNode;
begin
  inherited Create(ATerrain);
  Terrain := ATerrain;
  Layer := ALayer;

  TextureNode := TImageTextureNode.Create;
  TextureNode.SetUrl([WhitePixel]);
  TextureField := TSFNode.Create(Effect, false, 'tex_' + IntToStr(Layer), [], TextureNode);
  Effect.AddCustomField(TextureField);


  ColorField := TSFColor.Create(Effect, true, 'color_' + IntToStr(Layer), WhiteRGB);
  Effect.AddCustomField(ColorField);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleterrainlayer_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleTerrainLayer.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleterrainlayer_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleTerrainLayer.GetUsingPackedVector(const Vec: TSFVec4f): Single;
begin
  Result := Vec.Value[Layer - 1];
end;

procedure TCastleTerrainLayer.SetUsingPackedVector(const Vec: TSFVec4f; const Value: Single);
var
  V: TVector4;
begin
  V := Vec.Value;
  V.Data[Layer - 1] := Value;
  Vec.Send(V);
end;

function TCastleTerrainLayer.GetUvScale: Single;
begin
  Result := GetUsingPackedVector(Terrain.UvScaleField);
end;

procedure TCastleTerrainLayer.SetUvScale(const Value: Single);
begin
  SetUsingPackedVector(Terrain.UvScaleField, Value);
end;

function TCastleTerrainLayer.GetMetallic: Single;
begin
  Result := GetUsingPackedVector(Terrain.MetallicField);
end;

procedure TCastleTerrainLayer.SetMetallic(const Value: Single);
begin
  SetUsingPackedVector(Terrain.MetallicField, Value);
end;

function TCastleTerrainLayer.GetRoughness: Single;
begin
  Result := GetUsingPackedVector(Terrain.RoughnessField);
end;

procedure TCastleTerrainLayer.SetRoughness(const Value: Single);
begin
  SetUsingPackedVector(Terrain.RoughnessField, Value);
end;

function TCastleTerrainLayer.GetColor: TCastleColorRGB;
begin
  Result := ColorField.Value;
end;

procedure TCastleTerrainLayer.SetColor(const Value: TCastleColorRGB);
begin
  ColorField.Send(Value);
end;

function TCastleTerrainLayer.GetTexture: String;
begin
  if TextureNode.FdUrl.Count = 2 then // [image url, WhitePixel]
    Result := TextureNode.FdUrl.Items[0]
  else
    Result := '';
end;

procedure TCastleTerrainLayer.SetTexture(const Value: String);
begin
  if GetTexture() <> Value then
  begin
    if Value <> '' then
      TextureNode.SetUrl([Value, WhitePixel])
    else
      TextureNode.SetUrl([WhitePixel]);
    // TODO: Only this will properly update the shader uniform to use new texture
    Terrain.Scene.ChangedAll;
  end;
end;

function TCastleTerrainLayer.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'UvScale', 'ColorPersistent', 'Texture', 'Metallic', 'Roughness'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleterrainlayer_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleTerrain ------------------------------------------------------------- }

constructor TCastleTerrain.Create(AOwner: TComponent);

  procedure AdjustAppearance;
  var
    VertexPart, FragmentPart: TEffectPartNode;
    I: Integer;
  begin
    { initialize Effect node, for a shader effect }
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    { If no light sources are present, it is normal that almost all uniforms
      in this effect do not exist -- because they affect only
      the TPhysicalMaterialNode.BaseTexture, which is meaningless when no light shines. }
    Effect.UniformMissing := umIgnore;
    Appearance.SetEffects([Effect]);

    UvScaleField := TSFVec4f.Create(Effect, true, 'uv_scale', Vector4(
      TCastleTerrainLayer.DefaultUvScale,
      TCastleTerrainLayer.DefaultUvScale,
      TCastleTerrainLayer.DefaultUvScale,
      TCastleTerrainLayer.DefaultUvScale));
    Effect.AddCustomField(UvScaleField);

    MetallicField := TSFVec4f.Create(Effect, true, 'metallic', Vector4(
      TCastleTerrainLayer.DefaultMetallic,
      TCastleTerrainLayer.DefaultMetallic,
      TCastleTerrainLayer.DefaultMetallic,
      TCastleTerrainLayer.DefaultMetallic));
    Effect.AddCustomField(MetallicField);

    RoughnessField := TSFVec4f.Create(Effect, true, 'roughness', Vector4(
      TCastleTerrainLayer.DefaultRoughness,
      TCastleTerrainLayer.DefaultRoughness,
      TCastleTerrainLayer.DefaultRoughness,
      TCastleTerrainLayer.DefaultRoughness));
    Effect.AddCustomField(RoughnessField);

    for I := 1 to HeightsCount do
    begin
      HeightsFields[I] := TSFFloat.Create(Effect, true, 'height_' + IntToStr(I), DefaultHeight[I]);
      Effect.AddCustomField(HeightsFields[I]);
    end;

    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'layers_influence', FLayersInfluence));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'steep_emphasize', FSteepEmphasize));

    { initialize 2 EffectPart nodes (one for vertex shader, one for fragment shader) }
    FragmentPart := TEffectPartNode.Create;
    FragmentPart.ShaderType := stFragment;
    FragmentPart.Contents := {$I terrain.fs.inc};

    VertexPart := TEffectPartNode.Create;
    VertexPart.ShaderType := stVertex;
    VertexPart.Contents := {$I terrain.vs.inc};

    Effect.SetParts([FragmentPart, VertexPart]);

    { make the material lit }
    Appearance.Material := TPhysicalMaterialNode.Create;
  end;

var
  Layer: Cardinal;
begin
  inherited;

  FTriangulate := true;
  FSubdivisions := Vector2(DefaultSubdivisions, DefaultSubdivisions);
  FSize := Vector2(DefaultSize, DefaultSize);
  FLayersInfluence := DefaultLayersInfluence;
  FSteepEmphasize := DefaultSteepEmphasize;
  FPreciseCollisions := true;

  Scene := TCastleScene.Create(Self);
  //Scene.ProcessEvents := true; // not necessary right now for anything
  Scene.SetTransient;
  Scene.PreciseCollisions := true;
  Add(Scene);

  Appearance := TAppearanceNode.Create;
  Appearance.KeepExistingBegin; // it's easiest to manage release of Appearance
  AdjustAppearance;

  for Layer := 1 to LayersCount do
  begin
    Layers[Layer] := TCastleTerrainLayer.CreateForTerrain(Self, Layer, Effect);
    Layers[Layer].SetSubComponent(true);
    Layers[Layer].Name := 'Layer' + IntToStr(Layer);
  end;

  FDataObserver := TFreeNotificationObserver.Create(Self);
  FDataObserver.OnFreeNotification := {$ifdef FPC}@{$endif} DataFreeNotification;

  UpdateGeometry;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleterrain_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleTerrain.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleterrain_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
  // remove after RootNode containing this is removed too
  FreeAndNil(Appearance);
end;

procedure TCastleTerrain.Loaded;
begin
  inherited;
  if FUpdateGeometryWhenLoaded then
  begin
    FUpdateGeometryWhenLoaded := false;
    Assert(not IsLoading);
    UpdateGeometry;
  end;
end;

procedure TCastleTerrain.UpdateGeometry;

  function CreateQuadShape(const Range: TFloatRectangle): TAbstractChildNode;
  var
    QuadSet: TQuadSetNode;
    Coord: TCoordinateNode;
    Shape: TShapeNode;
    Transform: TTransformNode;
  begin
    Coord := TCoordinateNode.Create;
    Coord.SetPoint([
      { Order of these points matter, must be CCW when observed from top,
        just like "real" terrain data generated by TCastleTerrainNoise.
        This way backface culling works in the same way. }
      Vector3(          0, 0, Range.Height),
      Vector3(Range.Width, 0, Range.Height),
      Vector3(Range.Width, 0,            0),
      Vector3(          0, 0,            0)
    ]);

    QuadSet := TQuadSetNode.CreateWithTransform(Shape, Transform);
    QuadSet.Coord := Coord;

    Shape.Appearance := Appearance;

    { We translate the quad by TTransformNode, to keep the vertex coordinates
      in object space consistent with what is generated by CreateNode and CreateTriangulatedNode,
      and make the texture UV coordinates start the same. }
    Transform.Translation := Vector3(Range.Left, 0, Range.Bottom);

    Result := Transform;
  end;

  { Update associated collider, e.g. to update TCastleMeshCollider to reflect new terrain. }
  procedure UpdateCollider;
  var
    C: TCastleCollider;
  begin
    C := FindBehavior(TCastleCollider) as TCastleCollider;
    if C <> nil then
      C.InternalTransformChanged(Self);
  end;

var
  Root: TX3DRootNode;
  Range: TFloatRectangle;
begin
  { During deserialization, defer the real work to the Loaded moment,
    to avoid regenerating heights multiple times. }
  if IsLoading then
  begin
    FUpdateGeometryWhenLoaded := true;
    Exit;
  end;

  Range := FloatRectangle(-Size.X/2, -Size.Y/2, Size.X, Size.Y);

  if Data <> nil then
  begin
    if TerrainNode = nil then
    begin
      if Triangulate then
        TerrainNode := Data.CreateTriangulatedNode(Subdivisions, Range, Range, Appearance)
      else
        TerrainNode := Data.CreateNode(Subdivisions, Range, Range, Appearance);
      Root := TX3DRootNode.Create;
      Root.AddChildren(TerrainNode);
      Scene.Load(Root, true);
    end else
    begin
      if Triangulate then
        Data.UpdateTriangulatedNode(TerrainNode, Subdivisions, Range, Range)
      else
        Data.UpdateNode(TerrainNode, Subdivisions, Range, Range);
    end;
  end else
  begin
    { When Data is empty, show a simple quad to visualize Size of the terrain. }
    Root := TX3DRootNode.Create;
    Root.AddChildren(CreateQuadShape(Range));
    Scene.Load(Root, true);
    { Do not let subsequent calls to use TerrainNode as it was destroyed
      by Scene.Load above.
      Testcase: create TCastleTerrain and TCastleTerrainImage,
      assign TCastleTerrain.Data to TCastleTerrainImage, to nil, again to TCastleTerrainImage. }
    TerrainNode := nil;
  end;

  UpdateCollider;
end;

function TCastleTerrain.GetLayer(const Index: Integer): TCastleTerrainLayer;
begin
  Result := Layers[Index];
end;

function TCastleTerrain.GetRenderOptions: TCastleRenderOptions;
begin
  Result := Scene.RenderOptions;
end;

procedure TCastleTerrain.SetData(const Value: TCastleTerrainData);
begin
  if FData <> Value then
  begin
    { Ignore nonsense assignment done by FpRttiJson when this node references
      in JSON non-yet-known name. FpRttiJson then creates an instance of
      TCastleTerrainData and assigns it here -- a useless instance,
      that would actually cause trouble as it has abstract methods. }
    if (Value <> nil) and
       (Value.ClassType = TCastleTerrainData) then
      Exit;

    if FData <> nil then
      FData.RemoveChangeNotification({$ifdef FPC}@{$endif} DataChanged);
    FData := Value;
    FDataObserver.Observed := Value;
    if FData <> nil then
      FData.AddChangeNotification({$ifdef FPC}@{$endif} DataChanged);
    UpdateGeometry;
  end;
end;

procedure TCastleTerrain.DataChanged(Sender: TObject);
begin
  UpdateGeometry;
end;

procedure TCastleTerrain.DataFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Data := nil;
end;

procedure TCastleTerrain.SetTriangulate(const Value: Boolean);
begin
  if FTriangulate <> Value then
  begin
    // free the TerrainNode and RootNode, even if already created, to create new one
    if TerrainNode <> nil then
    begin
      Scene.RootNode := nil; // will free TerrainNode and RootNode
      TerrainNode := nil;
    end;
    FTriangulate := Value;
    UpdateGeometry;
  end;
end;

procedure TCastleTerrain.SetSubdivisions(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FSubdivisions, Value) then
  begin
    FSubdivisions := Value;
    UpdateGeometry;
  end;
end;

procedure TCastleTerrain.SetSize(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FSize, Value) then
  begin
    FSize := Value;
    UpdateGeometry;
  end;
end;

function TCastleTerrain.GetHeight(const Index: Integer): Single;
begin
  Result := HeightsFields[Index].Value;
end;

procedure TCastleTerrain.SetHeight(const Index: Integer; const Value: Single);
begin
  HeightsFields[Index].Send(Value);
end;

procedure TCastleTerrain.SetLayersInfluence(const Value: Single);
begin
  if FLayersInfluence <> Value then
  begin
    FLayersInfluence := Value;
    (Effect.Field('layers_influence') as TSFFloat).Send(Value);
  end;
end;

procedure TCastleTerrain.SetSteepEmphasize(const Value: Single);
begin
  if FSteepEmphasize <> Value then
  begin
    FSteepEmphasize := Value;
    (Effect.Field('steep_emphasize') as TSFFloat).Send(Value);
  end;
end;

procedure TCastleTerrain.SetPreciseCollisions(const Value: Boolean);
begin
  if FPreciseCollisions <> Value then
  begin
    FPreciseCollisions := Value;
    Scene.PreciseCollisions := Value;
  end;
end;

function TCastleTerrain.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'RenderOptions', 'Data', 'Triangulate', 'SubdivisionsPersistent',
       'SizePersistent', 'PreciseCollisions',
       'Height1', 'Height2',
       'Layer1', 'Layer2', 'Layer3', 'Layer4',
       'LayersInfluence', 'SteepEmphasize'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleTerrain.HasColliderMesh: Boolean;
begin
  Result := true;
end;

procedure TCastleTerrain.ColliderMesh(const TriangleEvent: TTriangleEvent);
begin
  Scene.ColliderMesh(TriangleEvent);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleterrain_persistent_vectors.inc}
{$undef read_implementation_methods}

initialization
  RegisterSerializableComponent(TCastleTerrainImage, ['Terrain Data (Experimental)', 'Image Data']);
  RegisterSerializableComponent(TCastleTerrainNoise, ['Terrain Data (Experimental)', 'Noise Data']);
  RegisterSerializableComponent(TCastleTerrainCombine, ['Terrain Data (Experimental)', 'Combine Data']);
  RegisterSerializableComponent(TCastleTerrain, ['Terrain (Experimental)']);
end.
