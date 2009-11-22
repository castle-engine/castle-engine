{
  Copyright 2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Elevation (terrain) implementations. }
unit Elevations;

interface

uses SysUtils, Classes, KambiScript;

type
  { Elevation (height for each X, Y) data. }
  TElevation = class
  public
    function Height(const X, Y: Single): Single; virtual; abstract;
  end;

  { Elevation (height for each X, Y) data calculated from KambiScript
    expression. At construction, pass FunctionExpression,
    that is KambiScript language expression calculating height
    based on X, Y. }
  TElevationKamScript = class(TElevation)
  private
    FXVariable, FYVariable: TKamScriptFloat;
    FFunction: TKamScriptExpression;
  public
    constructor Create(const FunctionExpression: string);
    destructor Destroy; override;
    function Height(const X, Y: Single): Single; override;
  end;

  TNoiseInterpolation = (niNone, niLinear, niCosine);
  TNoiseInterpolation2DMethod = function
    (const X, Y: Single; const NoiseIndex: Cardinal): Single;

  { Procedural terrain: elevation data from a procedural noise.

    "Synthesized noise" means it's not simply something random.
    We take the noise (integer noise, i.e. hash), smooth it
    (how well, and how fast --- see NoiseInterpolation), and add several
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

      @item(
        The simplest practical introduction to the idea I found is on
        [http://freespace.virgin.net/hugo.elias/models/m_perlin.htm].
        It describes how to get nice noise very easily, and my approach follows
        theirs.

        The article isn't perfect, for starters --- it doesn't actually
        describe Perlin noise as far as I understand :) (Perlin noise
        is "gradient noise" with some implementation hints; article describes
        "value noise"; the idea of cubic interpolation doesn't come from
        Perlin, AFAIK. Things specific about Perlin noise:
        [http://web.archive.org/web/20070706003038/http://www.cs.cmu.edu/~mzucker/code/perlin-noise-math-faq.html],
        [http://www.noisemachine.com/talk1/index.html].)
    )
  }
  TElevationNoise = class(TElevation)
  private
    FOctaves: Cardinal;
    FPersistence: Single;
    FAmplitude: Single;
    FFrequency: Single;
    FNoiseInterpolation: TNoiseInterpolation;
    FNoiseInterpolation2DMethod: TNoiseInterpolation2DMethod;
    procedure SetNoiseInterpolation(const Value: TNoiseInterpolation);
  public
    constructor Create;
    function Height(const X, Y: Single): Single; override;

    { Number of noise functions to sum.
      This linearly affects the time for Height call, so don't make
      it too much. Usually ~a few are Ok. }
    property Octaves: Cardinal read FOctaves write FOctaves default 4;

    { How amplitude changes, when frequency doubles. Default is 0.5. }
    property Persistence: Single read FPersistence write FPersistence default 0.5;

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

      TODO: one day cubic interpolation (using Catmull-Rom splines,
      which are special case of cubic Hermite spline, see
      http://en.wikipedia.org/wiki/Cubic_Hermite_spline,
      http://en.wikipedia.org/wiki/Bicubic_interpolation)
      should be implemented. I was planning it, but eventually cosine
      version turned out good and fast enough. }
    property NoiseInterpolation: TNoiseInterpolation
      read FNoiseInterpolation write SetNoiseInterpolation default niCosine;
  end;

  { Elevation data from a grid of values with specified width * height.
    Used when your underlying data is a simple 2D array of
    GridSizeX * GridSizeY heights.
    The idea is that on such elevation, there are special grid points
    where the height data is accurate. Everything else is an interpolation
    derived from this data. }
  TElevationGrid = class(TElevation)
  public
    { Get height of the elevation at specified 2D point.

      This is implemented in TElevationGrid class, using
      the data returned by GridHeight. For float X in 0..1 range,
      we return grid values for grid points 0..GridSizeX - 1.
      Outside 0..1 range, we clamp (that is, take nearest value
      from 0..1 range) --- this way the elevation seemingly continues
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
  end;

  TElevationSRTM = class(TElevationGrid)
  private
    FData: array [0..1200, 0..1200] of SmallInt;
  public
    constructor CreateFromFile(const FileName: string);

    function GridHeight(const X, Y: Cardinal): Single; override;
    function GridSizeX: Cardinal; override;
    function GridSizeY: Cardinal; override;
  end;

implementation

uses KambiUtils, KambiScriptParser, Noise;

{ TElevationKamScript -------------------------------------------------------- }

constructor TElevationKamScript.Create(const FunctionExpression: string);
begin
  inherited Create;

  FXVariable := TKamScriptFloat.Create(false);
  FXVariable.Name := 'x';
  FXVariable.OwnedByParentExpression := false;

  FYVariable := TKamScriptFloat.Create(false);
  FYVariable.Name := 'y';
  FYVariable.OwnedByParentExpression := false;

  FFunction := ParseFloatExpression(FunctionExpression, [FXVariable, FYVariable]);
end;

destructor TElevationKamScript.Destroy;
begin
  FFunction.FreeByParentExpression;
  FFunction := nil;

  FreeAndNil(FXVariable);
  FreeAndNil(FYVariable);

  inherited;
end;

function TElevationKamScript.Height(const X, Y: Single): Single;
begin
  FXVariable.Value := X;
  FYVariable.Value := Y;
  Result := (FFunction.Execute as TKamScriptFloat).Value;
end;

{ TElevationNoise ------------------------------------------------------------ }

procedure TElevationNoise.SetNoiseInterpolation(const Value: TNoiseInterpolation);
begin
  FNoiseInterpolation := Value;
  case Value of
    niNone: FNoiseInterpolation2DMethod := @InterpolatedNoise2D_None;
    niLinear: FNoiseInterpolation2DMethod := @InterpolatedNoise2D_Linear;
    niCosine: FNoiseInterpolation2DMethod := @InterpolatedNoise2D_Cosine;
    else raise EInternalError.Create('TElevationNoise.SetNoiseInterpolation(value?)');
  end;
end;

constructor TElevationNoise.Create;
begin
  inherited Create;
  FOctaves := 4;
  FPersistence := 0.5;
  FAmplitude := 1.0;
  FFrequency := 1.0;
  NoiseInterpolation := niCosine;
end;

function TElevationNoise.Height(const X, Y: Single): Single;
var
  A, F: Single;
  I: Cardinal;
begin
  Result := 0;
  A := Amplitude;
  F := Frequency;
  for I := 1 to Octaves do
  begin
    Result += FNoiseInterpolation2DMethod(X * F, Y * F, I) * A;
    F *= 2;
    A *= Persistence;
  end;
end;

{ TElevationGrid ------------------------------------------------------------- }

function TElevationGrid.Height(const X, Y: Single): Single;
begin
  { TODO: for now, just take the nearest point, no bilinear filtering. }
  Result := GridHeight(
    Clamped(Round(X * (GridSizeX - 1)), 0, GridSizeX - 1),
    Clamped(Round(Y * (GridSizeY - 1)), 0, GridSizeY - 1));
end;

{ TElevationSRTM ------------------------------------------------------------- }

constructor TElevationSRTM.CreateFromFile(const FileName: string);
var
  Stream: TFileStream;
  P: PSmallInt;
  I: Cardinal;
  LastCorrectHeight: SmallInt;
begin
  inherited Create;

  Stream := TFileStream.Create(FileName, fmOpenRead);
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

function TElevationSRTM.GridHeight(const X, Y: Cardinal): Single;
begin
  Result := FData[X, Y];
end;

function TElevationSRTM.GridSizeX: Cardinal;
begin
  Result := 1201;
end;

function TElevationSRTM.GridSizeY: Cardinal;
begin
  Result := 1201;
end;

end.
