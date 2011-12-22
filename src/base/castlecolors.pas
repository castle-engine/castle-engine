{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Color utilities, including HSV <-> RGB convertion. }
unit CastleColors;

interface

uses Math, VectorMath;

const
  { Some colors.
    3-item colors are in RGB format,
    4-item colors have additional 4th component always at maximum
    (1.0 for floats, 255 for bytes etc.)

    @groupBegin }
  Black3Byte  : TVector3Byte = (  0,   0,   0);
  Red3Byte    : TVector3Byte = (255,   0,   0);
  Green3Byte  : TVector3Byte = (  0, 255,   0);
  Blue3Byte   : TVector3Byte = (  0,   0, 255);
  White3Byte  : TVector3Byte = (255, 255, 255);

  Black4Byte  : TVector4Byte = (  0,   0,   0, 255);
  Red4Byte    : TVector4Byte = (255,   0,   0, 255);
  Green4Byte  : TVector4Byte = (  0, 255,   0, 255);
  Blue4Byte   : TVector4Byte = (  0,   0, 255, 255);
  White4Byte  : TVector4Byte = (255, 255, 255, 255);
  { @groupEnd }

  { Standard 16 colors.
    @groupBegin }
  Black3Single        : TVector3Single = (   0,    0,    0);
  Blue3Single         : TVector3Single = (   0,    0,  0.6);
  Green3Single        : TVector3Single = (   0,  0.6,    0);
  Cyan3Single         : TVector3Single = (   0,  0.6,  0.6);
  Red3Single          : TVector3Single = ( 0.6,    0,    0);
  Magenta3Single      : TVector3Single = ( 0.6,    0,  0.6);
  Brown3Single        : TVector3Single = ( 0.6,  0.3,    0);
  LightGray3Single    : TVector3Single = ( 0.6,  0.6,  0.6);
  DarkGray3Single     : TVector3Single = ( 0.3,  0.3,  0.3);
  LightBlue3Single    : TVector3Single = ( 0.3,  0.3,    1);
  LightGreen3Single   : TVector3Single = ( 0.3,    1,  0.3);
  LightCyan3Single    : TVector3Single = ( 0.3,    1,    1);
  LightRed3Single     : TVector3Single = (   1,  0.3,  0.3);
  LightMagenta3Single : TVector3Single = (   1,  0.3,    1);
  Yellow3Single       : TVector3Single = (   1,    1,  0.3);
  White3Single        : TVector3Single = (   1,    1,    1);
  { @groupEnd }

  { Some additional colors.
    @groupBegin }
  Gray3Single         : TVector3Single = ( 0.5,  0.5,  0.5);
  DarkGreen3Single    : TVector3Single = (   0,  0.3,    0);
  DarkBrown3Single    : TVector3Single = (0.63, 0.15,    0);
  Orange3Single       : TVector3Single = (   1,  0.5,    0);
  { @groupEnd }

  { 4-components versions of 3Single colors above.
    Just for your comfort (and some small speed gain sometimes),
    as opposed to calling Vector4Single(Xxx3Single) all the time.

    @groupBegin }
  Black4Single        : TVector4Single = (   0,    0,    0, 1);
  Blue4Single         : TVector4Single = (   0,    0,  0.6, 1);
  Green4Single        : TVector4Single = (   0,  0.6,    0, 1);
  Cyan4Single         : TVector4Single = (   0,  0.6,  0.6, 1);
  Red4Single          : TVector4Single = ( 0.6,    0,    0, 1);
  Magenta4Single      : TVector4Single = ( 0.6,    0,  0.6, 1);
  Brown4Single        : TVector4Single = ( 0.6,  0.3,    0, 1);
  LightGray4Single    : TVector4Single = ( 0.6,  0.6,  0.6, 1);
  DarkGray4Single     : TVector4Single = ( 0.3,  0.3,  0.3, 1);
  LightBlue4Single    : TVector4Single = ( 0.3,  0.3,    1, 1);
  LightGreen4Single   : TVector4Single = ( 0.3,    1,  0.3, 1);
  LightCyan4Single    : TVector4Single = ( 0.3,    1,    1, 1);
  LightRed4Single     : TVector4Single = (   1,  0.3,  0.3, 1);
  LightMagenta4Single : TVector4Single = (   1,  0.3,    1, 1);
  Yellow4Single       : TVector4Single = (   1,    1,  0.3, 1);
  White4Single        : TVector4Single = (   1,    1,    1, 1);
  { @groupEnd }

{ Calculate color intensity, as for converting color to grayscale.
  @groupBegin }
function GrayscaleValue(const v: TVector3Single): Single; overload;
function GrayscaleValue(const v: TVector3Double): Double; overload;
function GrayscaleValue(const v: TVector3Byte): Byte; overload;
{ @groupEnd }

function Grayscale(const v: TVector3Single): TVector3Single; overload;
function Grayscale(const v: TVector4Single): Tvector4Single; overload;
function Grayscale(const v: TVector3Byte): TVector3Byte; overload;

type
  { Function that processes RGB colors.
    These are used in Images.ImageModulate. }
  TColorModulatorSingleFunc = function (const Color: TVector3Single): TVector3Single;
  TColorModulatorByteFunc = function (const Color: TVector3Byte): TVector3Byte;

{ below are some functions that can be used as above
  TColorModulatorSingleFunc or TColorModulatorByteFunc values. }
{ }

{ Convert color to grayscale.
  @groupBegin }
function ColorGrayscaleByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Place color intensity (calculated like for grayscale)
  into the given color component. Set the other components zero.
  @groupBegin }
function ColorRedConvertByte(const Color: TVector3Byte): TVector3Byte;
function ColorGreenConvertByte(const Color: TVector3Byte): TVector3Byte;
function ColorBlueConvertByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Set color values for two other channels to 0.
  Note that it's something entirely different than
  ImageConvertToChannelTo1st: here we preserve original channel values,
  and remove values on two other channels.

  @groupBegin }
function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Converting between RGB and HSV.
  For HSV, we keep components as floating-point values,
  with hue in 0..6 range, saturation and value in 0..1.
  For RGB, one version keeps components as bytes (0..255 range),
  and the other as floating-point values (0..1 range).
  @groupBegin }
function HsvToRgb(const Value: TVector3Single): TVector3Single;
function RgbToHsv(const Value: TVector3Single): TVector3Single;
function RgbToHsv(const Value: TVector3Byte): TVector3Single;
function HsvToRgbByte(const Value: TVector3Single): TVector3Byte;
{ @groupEnd }

{ Given two colors in RGB, interpolate them in HSV space. }
function LerpRgbInHsv(const A: Single; const V1, V2: TVector3Single): TVector3Single;

implementation

uses CastleUtils;

{ grayscale ------------------------------------------------------------------ }

const
  { Weights to change RGB color to grayscale.

    Explanation: Grayscale color is just a color with red = green = blue.
    So the simplest convertion of RGB to grayscale is just to set
    all three R, G, B components to the average (R + G + B) / 3.
    But, since human eye is most sensitive to green, then to red,
    and least sensitive to blue, it's better to calculate this
    with some non-uniform weights. GrayscaleValuesXxx constants specify
    these weights.

    Taken from libpng manual (so there for further references).

    For GrayscaleByte, they should be used like

  @longCode(#
    (R * GrayscaleValuesByte[0] +
     G * GrayscaleValuesByte[1] +
     B * GrayscaleValuesByte[2]) div 256
  #)

    GrayscaleValuesByte[] are declared as Word type to force implicit convertion
    in above expression from Byte to Word, since you have to use Word range
    to temporarily hold Byte * Byte multiplication in expression above.

    @groupBegin }
  GrayscaleValuesFloat: array [0..2] of Float = (0.212671, 0.715160, 0.072169);
  GrayscaleValuesByte: array [0..2] of Word = (54, 183, 19);
  { @groupEnd }

function GrayscaleValue(const v: TVector3Single): Single;
begin
  result := GrayscaleValuesFloat[0]*v[0]+
            GrayscaleValuesFloat[1]*v[1]+
            GrayscaleValuesFloat[2]*v[2];
end;

function GrayscaleValue(const v: TVector3Double): Double;
begin
  result := GrayscaleValuesFloat[0]*v[0]+
            GrayscaleValuesFloat[1]*v[1]+
            GrayscaleValuesFloat[2]*v[2];
end;

function GrayscaleValue(const v: TVector3Byte): Byte;
begin
  result := (GrayscaleValuesByte[0]*v[0]+
             GrayscaleValuesByte[1]*v[1]+
             GrayscaleValuesByte[2]*v[2]) div 256;
end;

function Grayscale(const v: TVector3Single): TVector3Single;
begin
  Result[0] := GrayscaleValue(V);
  Result[1] := Result[0];
  Result[2] := Result[0];
end;

function Grayscale(const v: TVector4Single): TVector4Single;
var
  V3: TVector3Single absolute V;
begin
  Result[0] := GrayscaleValue(V3);
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := V[3];
end;

function Grayscale(const v: TVector3Byte): TVector3Byte;
begin
  Result[0] := GrayscaleValue(V);
  Result[1] := Result[0];
  Result[2] := Result[0];
end;

{ color changing ------------------------------------------------------------ }

function ColorGrayscaleByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result := Grayscale(Color);
end;

function ColorRedConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[0] := GrayscaleValue(Color);
  Result[1] := 0;
  Result[2] := 0;
end;

function ColorGreenConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[1] := GrayscaleValue(Color);
  Result[0] := 0;
  Result[2] := 0;
end;

function ColorBlueConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[2] := GrayscaleValue(Color);
  Result[0] := 0;
  Result[1] := 0;
end;

function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[0] := Color[0];
  Result[1] := 0;
  Result[2] := 0;
end;

function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[0] := 0;
  Result[1] := Color[1];
  Result[2] := 0;
end;

function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := Color[2];
end;

{ HSV stuff ------------------------------------------------------------------ }

function RgbToHsv(const Value: TVector3Single): TVector3Single;
var
  Chroma, V: Single;
begin
  V := Max(Value[0], Value[1], Value[2]);
  Result[2] := V;
  Chroma := V - Min(Value[0], Value[1], Value[2]);

  { Chroma and V are now in the same range as RGB components.
    Which means 0..1 right now, so already Ok. }

  if Chroma = 0 then
  begin
    Result[0] := 0;
    Result[1] := 0;
  end else
  begin
    { calculate hue }
    if V = Value[0] then
    begin
      Result[0] := (Value[1] - Value[2]) / Chroma;
      if Result[0] < 0 then Result[0] += 6.0;
    end else
    if V = Value[1] then
      Result[0] := (Value[2] - Value[0]) / Chroma + 2.0 else
      Result[0] := (Value[0] - Value[1]) / Chroma + 4.0;

    { calculate saturation }
    Result[1] := Chroma / V;
  end;
end;

function HsvToRgb(const Value: TVector3Single): TVector3Single;
var
  F, P, Q, T, V: Single;
begin
  F := Frac(Value[0]);

  { RGB component candidates }
  V := Value[2];
  P := V * (1 -  Value[1]);
  Q := V * (1 - (Value[1] * F));
  T := V * (1 - (Value[1] * (1 - F)));

  case Floor(Value[0]) of
    0, 6:begin Result[0] := V; Result[1] := T; Result[2] := P; end;
    1:   begin Result[0] := Q; Result[1] := V; Result[2] := P; end;
    2:   begin Result[0] := P; Result[1] := V; Result[2] := T; end;
    3:   begin Result[0] := P; Result[1] := Q; Result[2] := V; end;
    4:   begin Result[0] := T; Result[1] := P; Result[2] := V; end;
    else begin Result[0] := V; Result[1] := P; Result[2] := Q; end;
  end;
end;

function RgbToHsv(const Value: TVector3Byte): TVector3Single;
var
  ValueFloat: TVector3Single;
begin
  ValueFloat[0] := Value[0] / 255.0;
  ValueFloat[1] := Value[1] / 255.0;
  ValueFloat[2] := Value[2] / 255.0;
  Result := RgbToHsv(ValueFloat);
end;

function HsvToRgbByte(const Value: TVector3Single): TVector3Byte;
var
  ResultFloat: TVector3Single;
begin
  ResultFloat := HsvToRgb(Value);
  Result[0] := RoundClamp255(ResultFloat[0] * 255.0);
  Result[1] := RoundClamp255(ResultFloat[1] * 255.0);
  Result[2] := RoundClamp255(ResultFloat[2] * 255.0);
end;

function LerpRgbInHsv(const A: Single; const V1, V2: TVector3Single): TVector3Single;
var
  H1, H2, HOut: TVector3Single;
  HueDiff: Single;
begin
  H1 := RgbToHsv(V1);
  H2 := RgbToHsv(V2);

  { if one of the colors has saturation = 0, then resulting hue is copied
    from the other color, not interpolated. Otherwise,
    colors with saturation = 0 get hue = 0, which causes
    interpolation from something colorful (like blue) to black go through
    weird hue. }
  if H1[1] = 0 then
    HOut[0] := H2[0] else
  if H2[1] = 0 then
    HOut[0] := H1[0] else
  begin
    HueDiff := H2[0] - H1[0];
    if HueDiff > 3 then
    begin
      { from hue 1 to hue 2 go down through 0.0 }
      H2[0] -= 6;
      HOut[0] := H1[0] + A * (H2[0] - H1[0]);
      if HOut[0] < 0 then HOut[0] += 6;
    end else
    if HueDiff < -3 then
    begin
      { from hue 1 to hue 2 go up through 6.0 }
      H2[0] += 6;
      HOut[0] := H1[0] + A * (H2[0] - H1[0]);
      if HOut[0] > 6 then HOut[0] -= 6;
    end else
      { normal lerp when HueDiff inside [-3, 3] }
      HOut[0] := H1[0] + A * (H2[0] - H1[0]);
  end;

  { lerp on saturation and value is normal }
  HOut[1] := H1[1] + A * (H2[1] - H1[1]);
  HOut[2] := H1[2] + A * (H2[2] - H1[2]);

  Result := HsvToRgb(HOut);
end;

end.
