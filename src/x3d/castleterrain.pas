{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Terrain (height map) implementations. }
unit CastleTerrain;

{$I castleconf.inc}

interface

uses SysUtils, Classes, CastleScript, CastleImages, X3DNodes,
  CastleVectors;

type
  TTerrain = class;

  TColorFromHeightFunction =
    function (Terrain: TTerrain; Height: Single): TVector3Single;

  { Terrain (height for each X, Y) data. }
  TTerrain = class
  public
    function Height(const X, Y: Single): Single; virtual; abstract;
    { Create X3D node with the given terrain. }
    function CreateNode(const Dimensions: Cardinal;
      const Size: Single; const XRange, ZRange: TVector2Single;
      const ColorFromHeight: TColorFromHeightFunction): TShapeNode;
  end;

  { Terrain (height for each X, Y) data taken from intensities in an image.

    The image covers (ImageX1, ImageY1) ... (ImageX2, ImageY2)
    area in XY plane. If you ask for Height outside of this range,
    it is repeated infinitely (if ImageRepeat) or clamped (if not ImageRepeat).
    Image color (converted to grayscale) acts as height (scaled by
    ImageHeightScale).

    When image is not loaded, this always returns height = 0. }
  TTerrainImage = class(TTerrain)
  private
    { FImage = nil and FImageURL = '' when not loaded. }
    FImage: TGrayscaleImage;
    FImageURL: string;
    FImageHeightScale: Single;
    FImageRepeat: boolean;
    FImageX1, FImageX2, FImageY1, FImageY2: Single;
  public
    constructor Create;
    destructor Destroy; override;

    function Height(const X, Y: Single): Single; override;

    procedure LoadImage(const AImageURL: string);
    procedure ClearImage;
    property ImageURL: string read FImageURL;

    property ImageHeightScale: Single
      read FImageHeightScale write FImageHeightScale default 1.0;

    property ImageRepeat: boolean
      read FImageRepeat write FImageRepeat default false;

    property ImageX1: Single read FImageX1 write FImageX1 default -1;
    property ImageY1: Single read FImageY1 write FImageY1 default -1;
    property ImageX2: Single read FImageX2 write FImageX2 default 1;
    property ImageY2: Single read FImageY2 write FImageY2 default 1;
  end;

  { Terrain (height for each X, Y) data calculated from CastleScript
    expression. At construction, pass FunctionExpression,
    that is CastleScript language expression calculating height
    based on X, Y.

    This descends from TTerrainImage, so you add an image to
    your function result. }
  TTerrainCasScript = class(TTerrainImage)
  private
    FXVariable, FYVariable: TCasScriptFloat;
    FFunction: TCasScriptExpression;
  public
    constructor Create(const FunctionExpression: string);
    destructor Destroy; override;
    function Height(const X, Y: Single): Single; override;
  end;

  TNoiseInterpolation = (niNone, niLinear, niCosine, niSpline);
  TNoise2DMethod = function (const X, Y: Single; const Seed: Cardinal): Single;

  { Procedural terrain: data from a procedural noise.

    "Synthesized noise" means it's not simply something random.
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
    )

    This descends from TTerrainImage, so you can add an image to
    your function result, e.g. to flatten some specific generated area. }
  TTerrainNoise = class(TTerrainImage)
  private
    FOctaves: Single;
    FSmoothness: Single;
    FAmplitude: Single;
    FFrequency: Single;
    FInterpolation: TNoiseInterpolation;
    NoiseMethod: TNoise2DMethod;
    FBlur: boolean;
    FSeed: Cardinal;
    FHeterogeneous: Single;
    procedure SetInterpolation(const Value: TNoiseInterpolation);
    procedure SetBlur(const Value: boolean);
    procedure UpdateNoiseMethod;
  public
    constructor Create;
    function Height(const X, Y: Single): Single; override;

    { Number of noise functions to sum.
      This linearly affects the time for Height call, so don't make
      it too much. Usually ~a few are Ok.

      (The fact that it's a float is just a simple trick to allow smooth
      transitions from x to x+1. In fact, it's executed like
      Trunc(Octaves) * some noises + Frac(Octaves) * some last noise.) }
    property Octaves: Single read FOctaves write FOctaves default 4.0;

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
      the terrain is dominated by noise. }
    property Smoothness: Single read FSmoothness write FSmoothness default 2.0;

    { Amplitude and frequency of the first noise octave.
      Amplitude scales the height of the result, and Frequency scales
      the size of the bumps.
      @groupBegin }
    property Amplitude: Single read FAmplitude write FAmplitude default 1.0;
    property Frequency: Single read FFrequency write FFrequency default 1.0;
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
      read FInterpolation write SetInterpolation default niCosine;

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
    property Blur: boolean read FBlur write SetBlur default false;

    { Determines the random seeds used when generating the terrain. }
    property Seed: Cardinal read FSeed write FSeed default 0;

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
      in section 2.3.2.5 "A Large Scale Terrain Model"). }
    property Heterogeneous: Single
      read FHeterogeneous write FHeterogeneous default 0.0;
  end;

  { Terrain data from a grid of values with specified width * height.
    Used when your underlying data is a simple 2D array of
    GridSizeX * GridSizeY heights.
    The idea is that on such terrain, there are special grid points
    where the height data is accurate. Everything else is an interpolation
    derived from this data. }
  TTerrainGrid = class(TTerrain)
  private
    FGridX1, FGridX2, FGridY1, FGridY2, FGridHeightScale: Single;
  public
    constructor Create;

    { Get height of the terrain at specified 2D point.

      This is implemented in TTerrainGrid class, using
      the data returned by GridHeight. For float X in 0..1 range,
      we return grid values for grid points 0..GridSizeX - 1.
      Outside 0..1 range, we clamp (that is, take nearest value
      from 0..1 range) --- this way the terrain seemingly continues
      into the infinity.

      In comparison to GridHeight, it's (very slightly) slower,
      and it doesn't really present any more interesting information
      (in contrast to typical procedural terrain, where there can be always
      more and more detail at each level). }
    function Height(const X, Y: Single): Single; override;

    { GridSizeX, GridSizeY specify grid dimensions.
      Use GridHeight(0..GridSizeX - 1, 0..GridSizeY - 1) to get height
      at particular grid point.
      @groupBegin }
    function GridHeight(const X, Y: Cardinal): Single; virtual; abstract;
    function GridSizeX: Cardinal; virtual; abstract;
    function GridSizeY: Cardinal; virtual; abstract;
    { @groupEnd }

    { Specify where terrain is located, for @link(Height) method.
      These do not affect GridHeight method.
      @groupBegin }
    property GridX1: Single read FGridX1 write FGridX1 default 0;
    property GridY1: Single read FGridY1 write FGridY1 default 0;
    property GridX2: Single read FGridX2 write FGridX2 default 1;
    property GridY2: Single read FGridY2 write FGridY2 default 1;
    property GridHeightScale: Single read FGridHeightScale write FGridHeightScale default 1;
    { @groupEnd }
  end;

  TTerrainSRTM = class(TTerrainGrid)
  private
    FData: array [0..1200, 0..1200] of SmallInt;
  public
    constructor CreateFromFile(const URL: string);

    function GridHeight(const X, Y: Cardinal): Single; override;
    function GridSizeX: Cardinal; override;
    function GridSizeY: Cardinal; override;
  end;

implementation

uses CastleUtils, CastleScriptParser, CastleNoise, Math, CastleDownload;

{ TTerrain ------------------------------------------------------------------- }

function TTerrain.CreateNode(const Dimensions: Cardinal;
  const Size: Single; const XRange, ZRange: TVector2Single;
  const ColorFromHeight: TColorFromHeightFunction): TShapeNode;
var
  X, Z: Cardinal;
  Grid: TElevationGridNode;
  Appearance: TAppearanceNode;
  Color: TColorNode;
begin
  Result := TShapeNode.Create;

  Grid := TElevationGridNode.Create;
  Result.FdGeometry.Value := Grid;
  Grid.FdCreaseAngle.Value := 4; { > pi, to be perfectly smooth }
  Grid.FdXDimension.Value := Dimensions;
  Grid.FdZDimension.Value := Dimensions;
  Grid.FdXSpacing.Value := Size / (Dimensions - 1);
  Grid.FdZSpacing.Value := Size / (Dimensions - 1);
  Grid.FdHeight.Items.Count := Dimensions * Dimensions;

  Color := TColorNode.Create;
  Grid.FdColor.Value := Color;
  Color.FdColor.Items.Count := Dimensions * Dimensions;

  for X := 0 to Dimensions - 1 do
    for Z := 0 to Dimensions - 1 do
    begin
      Grid.FdHeight.Items.L[X + Z * Dimensions] := Height(
        MapRange(X, 0, Dimensions, XRange[0], XRange[1]),
        MapRange(Z, 0, Dimensions, ZRange[0], ZRange[1]));

      Color.FdColor.Items.L[X + Z * Dimensions] :=
        ColorFromHeight(Self, Grid.FdHeight.Items.L[X + Z * Dimensions]);
    end;

  Appearance := TAppearanceNode.Create;
  Result.Appearance := Appearance;

  { add any material, to be lit (even without shaders) }
  Appearance.Material := TMaterialNode.Create;
end;

{ TTerrainImage ------------------------------------------------------------ }

constructor TTerrainImage.Create;
begin
  inherited;
  FImageHeightScale := 1.0;
  FImageX1 := -1;
  FImageY1 := -1;
  FImageX2 :=  1;
  FImageY2 :=  1;
end;

destructor TTerrainImage.Destroy;
begin
  ClearImage;
  inherited;
end;

procedure TTerrainImage.LoadImage(const AImageURL: string);
var
  NewImage: TGrayscaleImage;
begin
  NewImage := CastleImages.LoadImage(AImageURL, [TGrayscaleImage]) as TGrayscaleImage;

  FreeAndNil(FImage);
  FImage := NewImage;
  FImageURL := AImageURL;
end;

procedure TTerrainImage.ClearImage;
begin
  FreeAndNil(FImage);
  FImageURL := '';
end;

function TTerrainImage.Height(const X, Y: Single): Single;
var
  PX, PY: Integer;
begin
  if FImage <> nil then
  begin
    PX := Floor( ((X - ImageX1) / (ImageX2 - ImageX1)) * FImage.Width );
    PY := Floor( ((Y - ImageY1) / (ImageY2 - ImageY1)) * FImage.Height);

    if ImageRepeat then
    begin
      PX := PX mod FImage.Width;
      PY := PY mod FImage.Height;
      if PX < 0 then PX += FImage.Width;
      if PY < 0 then PY += FImage.Height;
    end else
    begin
      ClampVar(PX, 0, FImage.Width  - 1);
      ClampVar(PY, 0, FImage.Height - 1);
    end;

    Result := (FImage.PixelPtr(PX, PY)^ / High(Byte)) * ImageHeightScale;
  end else
    Result := 0;
end;

{ TTerrainCasScript -------------------------------------------------------- }

constructor TTerrainCasScript.Create(const FunctionExpression: string);
begin
  inherited Create;

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

function TTerrainCasScript.Height(const X, Y: Single): Single;
begin
  Result := inherited;
  FXVariable.Value := X;
  FYVariable.Value := Y;
  Result += (FFunction.Execute as TCasScriptFloat).Value;
end;

{ TTerrainNoise ------------------------------------------------------------ }

constructor TTerrainNoise.Create;
begin
  inherited Create;
  FOctaves := 4.0;
  FSmoothness := 2.0;
  FAmplitude := 1.0;
  FFrequency := 1.0;
  FInterpolation := niCosine;
  FBlur := false;
  UpdateNoiseMethod;
end;

procedure TTerrainNoise.UpdateNoiseMethod;
begin
  if Blur then
    case Interpolation of
      niNone: NoiseMethod := @BlurredInterpolatedNoise2D_None;
      niLinear: NoiseMethod := @BlurredInterpolatedNoise2D_Linear;
      niCosine: NoiseMethod := @BlurredInterpolatedNoise2D_Cosine;
      niSpline: NoiseMethod := @BlurredInterpolatedNoise2D_Spline;
      else raise EInternalError.Create('TTerrainNoise.UpdateNoiseMethod(Interpolation?)');
    end else
    case Interpolation of
      niNone: NoiseMethod := @InterpolatedNoise2D_None;
      niLinear: NoiseMethod := @InterpolatedNoise2D_Linear;
      niCosine: NoiseMethod := @InterpolatedNoise2D_Cosine;
      niSpline: NoiseMethod := @InterpolatedNoise2D_Spline;
      else raise EInternalError.Create('TTerrainNoise.UpdateNoiseMethod(Interpolation?)');
    end
end;

procedure TTerrainNoise.SetInterpolation(const Value: TNoiseInterpolation);
begin
  FInterpolation := Value;
  UpdateNoiseMethod;
end;

procedure TTerrainNoise.SetBlur(const Value: boolean);
begin
  FBlur := Value;
  UpdateNoiseMethod;
end;

function TTerrainNoise.Height(const X, Y: Single): Single;
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
      Exit(NoiseMethod(X * F, Y * F, OctaveNumber + Seed) * A);

    NoiseAccumulator /= Heterogeneous;
    { Following Musgrave's dissertation, we should now force
      NoiseAccumulator to <0, 1> range.
      We know our NoiseMethod is always positive, and we require
      Amplitude, Heterogeneous and such to also be always positive.
      So we already know NoiseAccumulator is always >= 0. }
    MinVar(NoiseAccumulator, 1);

    NoiseAccumulator *= NoiseMethod(X * F, Y * F, OctaveNumber + Seed);

    Result := NoiseAccumulator * A;
  end;

var
  I: Cardinal;
begin
  Result := inherited;
  A := Amplitude;
  F := Frequency;
  { This will accumulate multiplication of noise octaves.
    Initial value is chosen so that at first step (I = 1)
    NoiseAccumulator will become 1.0, and then NoiseMethod() * A. }
  NoiseAccumulator := Heterogeneous;
  for I := 1 to Trunc(Octaves) do
  begin
    Result += NextOctave(I);
    F *= 2;
    A /= Smoothness;
  end;

  { Add last octave's remainder.
    Just like a normal octave, but multiply by Frac(Octaves). }
  Result += Frac(Octaves) * NextOctave(Trunc(Octaves) + 1);
end;

{ TTerrainGrid ------------------------------------------------------------- }

constructor TTerrainGrid.Create;
begin
  inherited;
  FGridX1 := 0;
  FGridY1 := 0;
  FGridX2 := 1;
  FGridY2 := 1;
  FGridHeightScale := 1;
end;

function TTerrainGrid.Height(const X, Y: Single): Single;
begin
  { TODO: for now, just take the nearest point, no bilinear filtering. }
  Result := GridHeight(
    Clamped(Round(MapRange(X, GridX1, GridX2, 0, GridSizeX - 1)), 0, GridSizeX - 1),
    Clamped(Round(MapRange(Y, GridY1, GridY2, 0, GridSizeY - 1)), 0, GridSizeY - 1)) * GridHeightScale;
end;

{ TTerrainSRTM ------------------------------------------------------------- }

constructor TTerrainSRTM.CreateFromFile(const URL: string);
var
  Stream: TStream;
  P: PSmallInt;
  I: Cardinal;
  LastCorrectHeight: SmallInt;
begin
  inherited Create;

  Stream := Download(URL, [soForceMemoryStream]);
  try
    Stream.ReadBuffer(FData, SizeOf(FData));
  finally FreeAndNil(Stream) end;

  LastCorrectHeight := 0; { any sensible value }
  P := @(FData[0, 0]);
  for I := 1 to 1201 * 1201 do
  begin
    {$ifdef ENDIAN_LITTLE}
    P^ := Swap(P^);
    {$endif ENDIAN_LITTLE}

    { Fix unknown data by setting to last correct seen value.
      Since we scan data cell-by-cell, in a row, this is in practice
      somewhat excusable approach. Of course, we could do something much better
      (filling unknown values by interpolating values from around). }
    if P^ = Low(SmallInt) then
      P^ := LastCorrectHeight else
      LastCorrectHeight := P^;

    Inc(P);
  end;
end;

function TTerrainSRTM.GridHeight(const X, Y: Cardinal): Single;
begin
  Result := FData[X, Y];
end;

function TTerrainSRTM.GridSizeX: Cardinal;
begin
  Result := 1201;
end;

function TTerrainSRTM.GridSizeY: Cardinal;
begin
  Result := 1201;
end;

end.
