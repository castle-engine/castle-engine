{
  Copyright 2009-2022 Michalis Kamburelis.

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

      Implement this, and also add a brush to edit splatmap in CGE editor.)

    @item(Adding vegetation to the terrain.

      Adding grass and other small things (that must be placed in large batches
      and density should be dynamic).

      Adding trees, boulders and other large things (their density should
      also be dynamic for fast rendering, but at close distance they are just
      3D models).

    @item(
      Add non-trivial rendering algorithm.
      Right now it is really the simplest possible approach to generate a mesh
      from heights with a simplest shader mixing a few texture layers.)
  )

  Smaller TODOs:

  @unorderedList(
    @item(
      Various Y should be renamed to Z?
      Hm. It's true (the 2nd terrain dimension maps to Z) and would follow X3D ElevationGrid.
      But then, it's weird to think of it as "Z" as some cases.
      And TVector2 2nd param remains Y.)
    @item(
      Randomization should be independent from compiler/platform/future.
      Let's just use CastleRandom?
      Or just store the final heights... this makes sense, as we also want to later allow editing terrains.)
    @item(TCastleTerrain configurable textures number.)
    @item(TCastleTerrain Use GridCount, GridStep instead of Subdivisions, Size?
      See wyrd-forest arguments.)
    @item(TCastleTerrain add property to define material type, like in primitives - unlit, physical, phong?)
  )
}
unit CastleTerrain;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleClassUtils, CastleScript, CastleImages, X3DNodes, CastleVectors,
  CastleRectangles, CastleTransform, CastleScene, X3DFields, CastleRenderOptions;

type
  { Terrain (height map) data that can be used for @link(TCastleTerrain.Data). }
  TCastleTerrainData = class(TCastleComponent)
  strict private
    FChangeNotifications: TNotifyEventList;
    procedure UpdateNodeCore(const Node: TAbstractChildNode;
      const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle;
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
    function CreateNode(const Divisions: Cardinal;
      const InputRange, OutputRange: TFloatRectangle;
      const Appearance: TAppearanceNode): TAbstractChildNode; overload;

    { Update a node created by @link(CreateNode)
      to the new terrain and it's settings.
      The appearance of the previously created node (in CreateNode) is preserved. }
    procedure UpdateNode(const Node: TAbstractChildNode;
      const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle);

    { Alternative version of @link(CreateNode) that creates a different shape.
      It's has little less quality (triangulation is not adaptive like
      for ElevationGrid), but updating it (by UpdateTriangulatedNode)
      is a little faster (than updating the CreateNode by UpdatNode).
      In practice, the speed gain is minimal, and this method will likely
      be removed at some point.

      The parameters have the same meaning as for @link(CreateNode),
      and resulting look should be the same. }
    function CreateTriangulatedNode(const Divisions: Cardinal;
      const InputRange, OutputRange: TFloatRectangle;
      const Appearance: TAppearanceNode): TAbstractChildNode;

    { Update a node created by @link(CreateTriangulatedNode)
      to the new terrain and it's settings. }
    procedure UpdateTriangulatedNode(const Node: TAbstractChildNode;
      const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle);
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

  { Scene showing a terrain with 3 layers of textures. }
  TCastleTerrain = class(TCastleTransform)
  public
    const
      { Texture layers to render this terrain. TODO: Should not be hardcoded here. }
      LayersCount = 3;
  strict private
    type
      TLayer = record
        UvScale: TSFFloat;
        TextureNode: TImageTextureNode;
      end;

    var
      Scene: TCastleScene;
      TerrainNode: TAbstractChildNode;
      Appearance: TAppearanceNode;
      Effect: TEffectNode;
      HeightsFields: array [0..LayersCount] of TSFFloat;
      Layers: array [1..LayersCount] of TLayer;
      FData: TCastleTerrainData;
      FDataObserver: TFreeNotificationObserver;
      FTriangulate: Boolean;
      FSize: Single;
      FTextureMix: Single;
      FNormalDark: Single;
      FNormalDarkening: Single;
      FSubdivisions: Cardinal;
      FPreciseCollisions: Boolean;
      FUpdateGeometryWhenLoaded: Boolean;

    function GetRenderOptions: TCastleRenderOptions;
    procedure DataFreeNotification(const Sender: TFreeNotificationObserver);
    procedure DataChanged(Sender: TObject);

    { Regenerate geometry (vertexes, normals etc.) to show the current Data
      with current parameters. }
    procedure UpdateGeometry;

    procedure SetData(const Value: TCastleTerrainData);
    procedure SetTriangulate(const Value: Boolean);
    procedure SetSubdivisions(const Value: Cardinal);
    procedure SetSize(const Value: Single);
    function GetHeight(const Index: Integer): Single;
    procedure SetHeight(const Index: Integer; const Value: Single);
    function GetUvScale(const Index: Integer): Single;
    procedure SetUvScale(const Index: Integer; const Value: Single);
    function GetTexture(const Index: Integer): String;
    procedure SetTexture(const Index: Integer; const Value: String);
    procedure SetTextureMix(const Value: Single);
    procedure SetNormalDark(const Value: Single);
    procedure SetNormalDarkening(const Value: Single);
    procedure SetPreciseCollisions(const Value: Boolean);
  protected
    procedure Loaded; override;
  public
    const
      DefaultSubdivisions = 64;
      DefaultSize = 100;
      DefaultHeight0 = 5.0;
      DefaultHeight1 = 6.0;
      DefaultHeight2 = 7.0;
      DefaultHeight3 = 10.0;
      { Default values for @link(Height0), @link(Height1) etc.

        Note: This array duplicates information in constants
        @link(DefaultHeight0), @link(DefaultHeight1) etc.
        Unfortunately we need the simple constants too, to specify properties default values
        like "default DefaultHeight0". Using "default DefaultHeight[0]" doesn't work in FPC
        (nor in Delphi, but that's because Delphi cannot handle Single defaults at all). }
      DefaultHeight: array [0..LayersCount] of Single = (
        DefaultHeight0,
        DefaultHeight1,
        DefaultHeight2,
        DefaultHeight3
      );
      DefaultUvScale = 1.0;
      DefaultTextureMix = 1.0;
      DefaultNormalDark = 0.90;
      DefaultNormalDarkening = 0.5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property RenderOptions: TCastleRenderOptions read GetRenderOptions;

    { Data for terrain heights.
      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Data: TCastleTerrainData read FData write SetData;

    { Do we generate as a set of triangles, or ElevationGrid.
      TODO: this is internal decision, should behave always like true?

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Triangulate: Boolean read FTriangulate write SetTriangulate default true;

    { How dense is the mesh. It will have Subdivisions * Subdivisions vertexes.
      TODO: split into X, Z subdivisions.

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Subdivisions: Cardinal read FSubdivisions write SetSubdivisions default DefaultSubdivisions;

    { Size in X and Z.
      TODO: split into X, Z size, TVector2, just like TCastlePlane.

      Changing this requires rebuild of terrain geometry, so it's costly.
      Avoid doing it at runtime. }
    property Size: Single read FSize write SetSize {$ifdef FPC}default DefaultSize{$endif};

    { Below this height we show only Texture1. }
    property Height0: Single index 0 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight0{$endif};

    { Between Height0 and Height1, we mix Texture1 with Texture2. }
    property Height1: Single index 1 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight1{$endif};

    { Between Height1 and Height2, we show only Texture2. }
    property Height2: Single index 2 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight2{$endif};

    { Between Height2 and Height3, we mix Texture2 with Texture3.
      Above Height3 we show only Texture3. }
    property Height3: Single index 3 read GetHeight write SetHeight {$ifdef FPC}default DefaultHeight3{$endif};

    { Scale the Texture1. }
    property UvScale1: Single index 1 read GetUvScale write SetUvScale {$ifdef FPC}default DefaultUvScale{$endif};
    { Scale the Texture1. }
    property UvScale2: Single index 2 read GetUvScale write SetUvScale {$ifdef FPC}default DefaultUvScale{$endif};
    { Scale the Texture1. }
    property UvScale3: Single index 3 read GetUvScale write SetUvScale {$ifdef FPC}default DefaultUvScale{$endif};

    { 1st texture URL, used (maybe mixed with other textures) when terrain height is below Height1. }
    property Texture1: String index 1 read GetTexture write SetTexture;
    { 2st texture URL, used (maybe mixed with other textures) when terrain height is between Height0 and Height3. }
    property Texture2: String index 2 read GetTexture write SetTexture;
    { 3rd texture URL, used (maybe mixed with other textures) when terrain height is above Height2. }
    property Texture3: String index 3 read GetTexture write SetTexture;

    { How much do the textures affect the final color.
      0.0 means that textures are ignored, 1.0 means maximum influence. }
    property TextureMix: Single read FTextureMix write SetTextureMix
      {$ifdef FPC}default DefaultTextureMix{$endif};

    { The steep slope at which we make color maximally darker.
      The slope is just Y coordinate of the normalized normal vector.
      NormalDark = 0.0 means that we apply darkness only when slope is really vertical
      (it practically makes the darkness non-existing),
      1.0 means that we start applying the darkness when scope is even horizontal
      (it makes everything covered by darkness).

      TODO: Better name for this and NormalDarkening?
      DarknessStartSlope
      DarknessIntensity
    }
    property NormalDark: Single read FNormalDark write SetNormalDark
      {$ifdef FPC}default DefaultNormalDark{$endif};

    { How darker can we make the color, because of steep slope.
      0.0 means we can make it completely black, 1.0 disables color darkening. }
    property NormalDarkening: Single read FNormalDarkening write SetNormalDarkening
      {$ifdef FPC}default DefaultNormalDarkening{$endif};

    { Resolve collisions precisely with the terrain geometry.
      When this is @false we will only consider the terrain bounding box for collisions,
      which prevents moving on terrain nicely, picking terrain points with mouse etc.
      This sets @link(TCastleSceneCore.Spatial). }
    property PreciseCollisions: Boolean read FPreciseCollisions write SetPreciseCollisions default true;
  end experimental;

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
    WritelnWarning('FChangeNotifications already freed when RemoveFontSizeChangeNotification');
    { In case it may be nil when destroying, and we make notification.
      If this ever happens, in valid circumstances, we'll just remove the warning. }
    Exit;
  end;
  FChangeNotifications.Remove(Notify);
end;

function TCastleTerrainData.CreateNode(const Divisions: Cardinal;
  const InputRange, OutputRange: TFloatRectangle;
  const Appearance: TAppearanceNode): TAbstractChildNode;
begin
  Result := TTransformNode.Create;
  UpdateNodeCore(Result, Divisions, InputRange, OutputRange, Appearance);
end;

procedure TCastleTerrainData.UpdateNode(const Node: TAbstractChildNode;
  const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle);
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
  UpdateNodeCore(Node, Divisions, InputRange, OutputRange, Appearance);
  Appearance.KeepExistingEnd;
end;

procedure TCastleTerrainData.UpdateNodeCore(const Node: TAbstractChildNode;
  const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle;
  const Appearance: TAppearanceNode);
var
  Transform: TTransformNode;
  Shape: TShapeNode;
  Grid: TElevationGridNode;
  X, Z: Cardinal;
  Coord, TexCoord: TVector2;
begin
  Transform := Node as TTransformNode; // created by CreateNode
  Transform.ClearChildren;
  Transform.Translation := Vector3(OutputRange.Left, 0, OutputRange.Bottom);

  Shape := TShapeNode.Create;

  Grid := TElevationGridNode.Create;
  Shape.FdGeometry.Value := Grid;
  Grid.FdCreaseAngle.Value := 4; { > pi, to be perfectly smooth }
  Grid.FdXDimension.Value := Divisions;
  Grid.FdZDimension.Value := Divisions;
  Grid.FdXSpacing.Value := OutputRange.Width / (Divisions - 1);
  Grid.FdZSpacing.Value := OutputRange.Height / (Divisions - 1);
  Grid.FdHeight.Items.Count := Divisions * Divisions;

  for X := 0 to Divisions - 1 do
    for Z := 0 to Divisions - 1 do
    begin
      TexCoord := Vector2(
        MapRangeTo01(X, 0, Divisions - 1),
        MapRangeTo01(Z, 0, Divisions - 1)
      );
      Coord := Vector2(
        Lerp(TexCoord.X, InputRange.Left  , InputRange.Right),
        Lerp(TexCoord.Y, InputRange.Bottom, InputRange.Top)
      );
      // TexCoord.Y grows in different direction, see Height docs for reason
      TexCoord.Y := 1 - TexCoord.Y;
      Grid.FdHeight.Items.List^[X + Z * Divisions] := Height(Coord, TexCoord);
    end;

  Shape.Appearance := Appearance;

  // at the end, as this may cause Scene.ChangedAll
  Transform.AddChildren(Shape);
end;

function TCastleTerrainData.CreateTriangulatedNode(const Divisions: Cardinal;
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

  UpdateTriangulatedNode(Result, Divisions, InputRange, OutputRange);
end;

procedure TCastleTerrainData.UpdateTriangulatedNode(const Node: TAbstractChildNode;
  const Divisions: Cardinal; const InputRange, OutputRange: TFloatRectangle);
var
  DivisionsPlus1: Cardinal;
  Coord, Normal: TVector3List;
  Index: TLongIntList;
  FaceNormals: TVector3List;

  procedure CalculatePosition(const I, J: Cardinal; out Position: TVector3);
  var
    TexCoord, Coord: TVector2;
  begin
    TexCoord.X := I / (Divisions-1);
    TexCoord.Y := J / (Divisions-1);
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
    Result := I + J * DivisionsPlus1;
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
  IndexPtr: PLongInt;
begin
  { extract nodes from Node, assuming it was created by CreateTriangulatedNode }
  Transform := Node as TTransformNode;
  Shape := Transform.FdChildren[0] as TShapeNode;
  Geometry := Shape.Geometry as TIndexedTriangleStripSetNode;
  CoordNode := Geometry.Coord as TCoordinateNode;
  NormalNode := Geometry.Normal as TNormalNode;

  Transform.Translation := Vector3(OutputRange.Left, 0, OutputRange.Bottom);

  { Divisions-1 squares (edges) along the way,
    Divisions points along the way.
    Calculate positions for Divisions + 1 points
    (+ 1 additional for normal calculation). }
  DivisionsPlus1 := Divisions + 1;

  Index := Geometry.FdIndex.Items;
  Coord := CoordNode.FdPoint.Items;
  Normal := NormalNode.FdVector.Items;

  { We will render Divisions^2 points, but we want to calculate
    (Divisions + 1)^2 points : to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  Coord.Count := Sqr(DivisionsPlus1);
  Normal.Count := Sqr(DivisionsPlus1);

  { calculate Coord }
  for I := 0 to Divisions do
    for J := 0 to Divisions do
      CalculatePosition(I, J, Coord.List^[Idx(I, J)]);
  CoordNode.FdPoint.Changed;

  { calculate Normals }
  FaceNormals := TVector3List.Create;
  try
    FaceNormals.Count := Sqr(DivisionsPlus1);
    { calculate per-face (flat) normals }
    for I := 0 to Divisions - 1 do
      for J := 0 to Divisions - 1 do
        CalculateFaceNormal(I, J, FaceNormals.List^[Idx(I, J)]);
    { calculate smooth vertex normals }
    for I := 0 to Divisions - 1 do
      for J := 0 to Divisions - 1 do
        CalculateNormal(I, J, Normal.List^[Idx(I, J)]);
  finally FreeAndNil(FaceNormals) end;
  NormalNode.FdVector.Changed;

  { calculate Index }
  Index.Count := (Divisions - 1) * (Divisions * 2 + 1);
  IndexPtr := PLongInt(Index.List);
  for I := 1 to Divisions - 1 do
  begin
    for J := 0 to Divisions - 1 do
    begin
      // order to make it CCW when viewed from above
      IndexPtr^ := Idx(I    , J); Inc(IndexPtr);
      IndexPtr^ := Idx(I - 1, J); Inc(IndexPtr);
    end;
    IndexPtr^ := -1;
    Inc(IndexPtr);
  end;
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

procedure TCastleTerrainCombine.SetData1(const Value: TCastleTerrainData);
begin
  if FData1 <> Value then
  begin
    { Ignore nonsense assignment done by FpRttiJson when this node references
      in JSON non-yet-known name. See TCastleTerrain.SetData comments. }
    if (Value <> nil) and
       (Value.ClassType = TCastleTerrainData) then
      Exit;

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

    if FData2 <> nil then
      FData2.RemoveChangeNotification({$ifdef FPC}@{$endif} Data2Changed);
    FData2 := Value;
    FData2Observer.Observed := Value;
    if FData2 <> nil then
      FData2.AddChangeNotification({$ifdef FPC}@{$endif} Data2Changed);
    DoChange;
  end;
end;

{ TCastleTerrain ------------------------------------------------------------- }

const
  { URL of a white pixel texture, embedded PPM.
    See http://netpbm.sourceforge.net/doc/ppm.html . }
  WhitePixel = 'data:image/x-portable-pixmap,P3'#10 +
    '1 1'#10 +
    '255'#10 +
    '255 255 255';

constructor TCastleTerrain.Create(AOwner: TComponent);

  procedure AdjustAppearance;
  var
    VertexPart, FragmentPart: TEffectPartNode;
    Layer: Integer;
    TextureField: TSFNode;
  begin
    { initialize Effect node, for a shader effect }
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Appearance.SetEffects([Effect]);

    for Layer := 1 to LayersCount do
    begin
      Layers[Layer].TextureNode := TImageTextureNode.Create;
      Layers[Layer].TextureNode.SetUrl([WhitePixel]);
      TextureField := TSFNode.Create(Effect, false, 'tex_' + IntToStr(Layer), [], Layers[Layer].TextureNode);
      Effect.AddCustomField(TextureField);

      Layers[Layer].UvScale := TSFFloat.Create(Effect, true, 'uv_scale_' + IntToStr(Layer), DefaultUvScale);
      Effect.AddCustomField(Layers[Layer].UvScale);
    end;

    for Layer := 0 to LayersCount do
    begin
      HeightsFields[Layer] := TSFFloat.Create(Effect, true, 'h' + IntToStr(Layer), DefaultHeight[Layer]);
      Effect.AddCustomField(HeightsFields[Layer]);
    end;

    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'texture_mix', FTextureMix));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_dark', FNormalDark));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_darkening', FNormalDarkening));

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

begin
  inherited;

  FTriangulate := true;
  FSubdivisions := DefaultSubdivisions;
  FSize := DefaultSize;
  FTextureMix := DefaultTextureMix;
  FNormalDark := DefaultNormalDark;
  FNormalDarkening := DefaultNormalDarkening;
  FPreciseCollisions := true;

  Scene := TCastleScene.Create(Self);
  //Scene.ProcessEvents := true; // not necessary right now for anything
  Scene.SetTransient;
  Scene.Spatial := [ssDynamicCollisions]; // following FPreciseCollisions = true
  Add(Scene);

  Appearance := TAppearanceNode.Create;
  Appearance.KeepExistingBegin; // it's easiest to manage release of Appearance
  AdjustAppearance;

  FDataObserver := TFreeNotificationObserver.Create(Self);
  FDataObserver.OnFreeNotification := {$ifdef FPC}@{$endif} DataFreeNotification;

  UpdateGeometry;
end;

destructor TCastleTerrain.Destroy;
begin
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

  Range := FloatRectangle(-Size/2, -Size/2, Size, Size);

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

procedure TCastleTerrain.SetSubdivisions(const Value: Cardinal);
begin
  if FSubdivisions <> Value then
  begin
    FSubdivisions := Value;
    UpdateGeometry;
  end;
end;

procedure TCastleTerrain.SetSize(const Value: Single);
begin
  if FSize <> Value then
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

function TCastleTerrain.GetUvScale(const Index: Integer): Single;
begin
  Result := Layers[Index].UvScale.Value;
end;

procedure TCastleTerrain.SetUvScale(const Index: Integer; const Value: Single);
begin
  Layers[Index].UvScale.Send(Value);
end;

function TCastleTerrain.GetTexture(const Index: Integer): String;
begin
  if Layers[Index].TextureNode.FdUrl.Count = 2 then // [image url, WhitePixel]
    Result := Layers[Index].TextureNode.FdUrl.Items[0]
  else
    Result := '';
end;

procedure TCastleTerrain.SetTexture(const Index: Integer; const Value: String);
begin
  if GetTexture(Index) <> Value then
  begin
    if Value <> '' then
      Layers[Index].TextureNode.SetUrl([Value, WhitePixel])
    else
      Layers[Index].TextureNode.SetUrl([WhitePixel]);
    // TODO: Only this will properly update the shader uniform to use new texture
    Scene.ChangedAll;
  end;
end;

procedure TCastleTerrain.SetTextureMix(const Value: Single);
begin
  if FTextureMix <> Value then
  begin
    FTextureMix := Value;
    (Effect.Field('texture_mix') as TSFFloat).Send(Value);
  end;
end;

procedure TCastleTerrain.SetNormalDark(const Value: Single);
begin
  if FNormalDark <> Value then
  begin
    FNormalDark := Value;
    (Effect.Field('normal_dark') as TSFFloat).Send(Value);
  end;
end;

procedure TCastleTerrain.SetNormalDarkening(const Value: Single);
begin
  if FNormalDarkening <> Value then
  begin
    FNormalDarkening := Value;
    (Effect.Field('normal_darkening') as TSFFloat).Send(Value);
  end;
end;

procedure TCastleTerrain.SetPreciseCollisions(const Value: Boolean);
begin
  if FPreciseCollisions <> Value then
  begin
    FPreciseCollisions := Value;
    if Value then
      Scene.Spatial := [ssDynamicCollisions]
    else
      Scene.Spatial := [];
    { Note that we don't add ssRendering,
      would be largely useless as primitives are usually just 1 shape in an internal scene. }
  end;
end;

function TCastleTerrain.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'RenderOptions', 'Data', 'Triangulate', 'Subdivisions', 'Size', 'PreciseCollisions',
       'Height0', 'Height1', 'Height2', 'Height3',
       'UvScale1', 'UvScale2', 'UvScale3',
       'Texture1', 'Texture2', 'Texture3',
       'TextureMix', 'NormalDark', 'NormalDarkening'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TCastleTerrainImage, 'Terrain Data (Experimental)/Image Data');
  RegisterSerializableComponent(TCastleTerrainNoise, 'Terrain Data (Experimental)/Noise Data');
  RegisterSerializableComponent(TCastleTerrainCombine, 'Terrain Data (Experimental)/Combine Data');
  RegisterSerializableComponent(TCastleTerrain, 'Terrain (Experimental)/Terrain');
end.
