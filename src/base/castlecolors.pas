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

{ Color utilities, including HSV <-> RGB conversion. }
unit CastleColors;

{$include castleconf.inc}

interface

uses Math, Classes,
  CastleVectors, CastleClassUtils;

type
  TCastleColor = TVector4;
  PCastleColor = PVector4;
  TCastleColorRGB = TVector3;
  PCastleColorRGB = PVector3;

  TCastleColorHelper = record helper for TVector4
    { Get first 3 color components, ignore alpha. }
    function RGB: TCastleColorRGB;
    { Return color converted into grayscale. Alpha is left unchanged. }
    function Grayscale: TCastleColor;
  end;

  TCastleColorRGBHelper = record helper for TVector3
    { Return color converted into grayscale. }
    function Grayscale: TCastleColorRGB;
  end;

const
  { Common color constants, for comfort.
    They follow the CSS colors constants
    [http://www.w3.org/TR/CSS21/syndata.html#color-units].
    @groupBegin }
  Maroon : TCastleColor = (X: 0.5; Y: 0.0; Z: 0.0; W: 1.0);
  Red    : TCastleColor = (X: 1.0; Y: 0.0; Z: 0.0; W: 1.0);
  Orange : TCastleColor = (X: 1.0; Y: 0.65; Z: 0.0; W: 1.0);
  Yellow : TCastleColor = (X: 1.0; Y: 1.0; Z: 0.0; W: 1.0);
  Olive  : TCastleColor = (X: 0.5; Y: 0.5; Z: 0.0; W: 1.0);
  Purple : TCastleColor = (X: 0.5; Y: 0.0; Z: 0.5; W: 1.0);
  Fuchsia: TCastleColor = (X: 1.0; Y: 0.0; Z: 1.0; W: 1.0);
  White  : TCastleColor = (X: 1.0; Y: 1.0; Z: 1.0; W: 1.0);
  Lime   : TCastleColor = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);
  Green  : TCastleColor = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);
  Navy   : TCastleColor = (X: 0.0; Y: 0.0; Z: 0.5; W: 1.0);
  Blue   : TCastleColor = (X: 0.0; Y: 0.0; Z: 1.0; W: 1.0);
  Aqua   : TCastleColor = (X: 0.0; Y: 1.0; Z: 1.0; W: 1.0);
  Teal   : TCastleColor = (X: 0.0; Y: 0.5; Z: 0.5; W: 1.0);
  Black  : TCastleColor = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);
  Silver : TCastleColor = (X: 0.75; Y: 0.75; Z: 0.75; W: 1.0);
  Gray   : TCastleColor = (X: 0.5; Y: 0.5; Z: 0.5; W: 1.0);
  { @groupEnd }

  { Additional color constants. } { }
  LightRed  : TCastleColor = (X: 1.0; Y: 0.33; Z: 0.33; W: 1.0);
  LightGreen: TCastleColor = (X: 0.33; Y: 1.0; Z: 0.33; W: 1.0);
  LightBlue : TCastleColor = (X: 0.33; Y: 0.33; Z: 1.0; W: 1.0);

  WhiteRGB  : TCastleColorRGB = (X: 1.0; Y: 1.0; Z: 1.0);
  BlackRGB  : TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);
  RedRGB    : TCastleColorRGB = (X: 1.0; Y: 0.0; Z: 0.0);
  YellowRGB : TCastleColorRGB = (X: 1.0; Y: 1.0; Z: 0.0);
  GreenRGB  : TCastleColorRGB = (X: 0.0; Y: 1.0; Z: 0.0);
  BlueRGB   : TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 1.0);
  GrayRGB   : TCastleColorRGB = (X: 0.5; Y: 0.5; Z: 0.5);

  { Deprecated, use WhiteRGB. @deprecated }
  White3Single  : TCastleColorRGB = (X: 1.0; Y: 1.0; Z: 1.0) deprecated;
  { Deprecated, use BlackRGB. @deprecated }
  Black3Single  : TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0) deprecated;
  { Deprecated, use RedRGB. @deprecated }
  Red3Single    : TCastleColorRGB = (X: 1.0; Y: 0.0; Z: 0.0) deprecated;
  { Deprecated, use GreenRGB. @deprecated }
  Green3Single  : TCastleColorRGB = (X: 0.0; Y: 1.0; Z: 0.0) deprecated;
  { Deprecated, use BlueRGB. @deprecated }
  Blue3Single   : TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 1.0) deprecated;

{ Calculate color intensity, for converting color to grayscale.
  @groupBegin }
function GrayscaleValue(const v: TCastleColorRGB): Single; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TCastleColor): Single; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TVector3Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TVector4Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
{ @groupEnd }

function Grayscale(const v: TCastleColorRGB): TCastleColorRGB; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function Grayscale(const v: TVector3Byte): TVector3Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function Grayscale(const v: TCastleColor): TCastleColor; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;

type
  { Function that processes RGB colors, used by TCastleImage.ModulateRGB. }
  TColorModulatorByteFunc = function (const Color: TVector3Byte): TVector3Byte;

{ Some functions matching TColorModulatorByteFunc type. }

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
  ImageConvertToChannelVar: here we preserve original channel values,
  and remove values on two other channels.

  @groupBegin }
function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Converting between RGB and HSV.

  For HSV, we keep components as floating-point values,
  with hue in 0..6 range, saturation and value in 0..1.

  For RGB, we keep components as floating-point values (0..1 range).

  @groupBegin }
function HsvToRgb(const Value: TVector3): TCastleColorRGB;
function HsvToRgba(const Value: TVector3; const Alpha: Single): TCastleColor;
function RgbToHsv(const Value: TVector3): TVector3; overload;
{ @groupEnd }

function RgbToHsv(const Value: TVector3Byte): TVector3; overload; deprecated 'use float-based colors, like TCastleColorRGB and TCastleColor, not Byte-based like TVector3Byte';
function HsvToRgbByte(const Value: TVector3): TVector3Byte; deprecated 'use float-based colors, like TCastleColorRGB and TCastleColor, not Byte-based like TVector3Byte';

{ Given two colors in RGB, interpolate them in HSV space. }
function LerpRgbInHsv(const A: Single; const V1, V2: TVector3): TVector3;

{ Change color into a hexadecimal notation of it (like in HTML).
  This color includes an alpha channel (as 4th component),
  and so the output contains the alpha value at the end (so it's 8 hex digits),
  unless alpha is opaque in which case it's not written (and result is 6
  hex digits). }
function ColorToHex(const V: TCastleColor): string;

{ Change color into a hexadecimal notation of it (like in HTML).
  This color has no alpha channel,
  so it's always 6 hex digits. }
function ColorRGBToHex(const V: TCastleColorRGB): string;

{ Convert hexadecimal color notation (like in HTML) into an RGBA color.
  Handles 8 or 6 digit color (RGB or RGBA with 2 letters per component;
  for 6 digits, alpha is assumed to be 1.0 (opaque)).

  @raises EConvertError In case of invalid color as string. }
function HexToColor(const S: string): TCastleColor;

{ Convert hexadecimal color notation (like in HTML) into an RGB color.
  Handles 8 or 6 digit color (RGB or RGBA with 2 letters per component;
  for 8 digits, alpha is ignored).

  @raises EConvertError In case of invalid color as string. }
function HexToColorRGB(const S: string): TCastleColorRGB;

{ Change color opacity (alpha). }
function ColorOpacity(const Color: TCastleColor; const Opacity: Single): TCastleColor;

function FadeDarkColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
function FadeColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;

{$define read_interface}
{$I castlecolors_persistent.inc}
{$undef read_interface}

implementation

uses SysUtils, CastleUtils, CastleStringUtils;

{$define read_implementation}
{$I castlecolors_persistent.inc}
{$undef read_implementation}

{ TCastleColorHelper --------------------------------------------------------------- }

function TCastleColorHelper.RGB: TCastleColorRGB;
begin
  Result := XYZ;
end;

function TCastleColorHelper.Grayscale: TCastleColor;
begin
  Result := CastleColors.Grayscale(Self);
end;

{ TCastleColorRGBHelper ------------------------------------------------------------ }

function TCastleColorRGBHelper.Grayscale: TCastleColorRGB;
begin
  Result := CastleColors.Grayscale(Self);
end;

{ grayscale ------------------------------------------------------------------ }

function GrayscaleValue(const v: TCastleColor): Single;
begin
  { Weights to change RGB color to grayscale.

    Grayscale color is just a color with red = green = blue.
    So the simplest conversion of RGB to grayscale is just to set
    all three R, G, B components to the average (R + G + B) / 3.
    But, since human eye is most sensitive to green, then to red,
    and least sensitive to blue, it's better to calculate this
    with some non-uniform weights.

    These weights are copied from libpng manual. }

  Result := (0.212671 * V.X+
             0.715160 * V.Y+
             0.072169 * V.Z);
end;

function GrayscaleValue(const v: TCastleColorRGB): Single;
begin
  Result := 0.212671 * V.X+
            0.715160 * V.Y+
            0.072169 * V.Z;
end;

function GrayscaleValue(const v: TVector3Byte): Byte;
begin
  // force multiplication as Word
  Result := (Word(54 ) * V.X+
             Word(183) * V.Y+
             Word(19 ) * V.Z) shr 8; //div 256;
end;

function GrayscaleValue(const v: TVector4Byte): Byte;
begin
  Result := (Word(54 ) * V.X+
             Word(183) * V.Y+
             Word(19 ) * V.Z) shr 8; //div 256;
end;

function Grayscale(const v: TCastleColor): TCastleColor;
var
  V3: TVector3 absolute V;
begin
  Result.X := GrayscaleValue(V3);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := V.W;
end;

function Grayscale(const v: TCastleColorRGB): TCastleColorRGB;
begin
  Result.X := GrayscaleValue(V);
  Result.Y := Result.X;
  Result.Z := Result.X;
end;

function Grayscale(const v: TVector3Byte): TVector3Byte;
begin
  Result.X := GrayscaleValue(V);
  Result.Y := Result.X;
  Result.Z := Result.X;
end;

{ color changing ------------------------------------------------------------ }

function ColorGrayscaleByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result := Grayscale(Color);
end;

function ColorRedConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.X := GrayscaleValue(Color);
  Result.Y := 0;
  Result.Z := 0;
end;

function ColorGreenConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Y := GrayscaleValue(Color);
  Result.X := 0;
  Result.Z := 0;
end;

function ColorBlueConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Z := GrayscaleValue(Color);
  Result.X := 0;
  Result.Y := 0;
end;

function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.X := Color.X;
  Result.Y := 0;
  Result.Z := 0;
end;

function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.X := 0;
  Result.Y := Color.Y;
  Result.Z := 0;
end;

function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := Color.Z;
end;

{ HSV stuff ------------------------------------------------------------------ }

function RgbToHsv(const Value: TVector3): TVector3;
var
  Chroma, V: Single;
begin
  V := Value.Max;
  Result.Z := V;
  Chroma := V - Value.Min;

  { Chroma and V are now in the same range as RGB components.
    Which means 0..1 right now, so already Ok. }

  if Chroma = 0 then
  begin
    Result.X := 0;
    Result.Y := 0;
  end else
  begin
    { calculate hue }
    if V = Value.X then
    begin
      Result.X := (Value.Y - Value.Z) / Chroma;
      if Result.X < 0 then
        Result.X := Result.X + 6.0;
    end else
    if V = Value.Y then
      Result.X := (Value.Z - Value.X) / Chroma + 2.0 else
      Result.X := (Value.X - Value.Y) / Chroma + 4.0;

    { calculate saturation }
    Result.Y := Chroma / V;
  end;
end;

function HsvToRgb(const Value: TVector3): TCastleColorRGB;
var
  F, P, Q, T, V: Single;
begin
  F := Frac(Value.X);

  { RGB component candidates }
  V := Value.Z;
  P := V * (1 -  Value.Y);
  Q := V * (1 - (Value.Y * F));
  T := V * (1 - (Value.Y * (1 - F)));

  case Floor(Value.X) of
    0, 6:begin Result.X := V; Result.Y := T; Result.Z := P; end;
    1:   begin Result.X := Q; Result.Y := V; Result.Z := P; end;
    2:   begin Result.X := P; Result.Y := V; Result.Z := T; end;
    3:   begin Result.X := P; Result.Y := Q; Result.Z := V; end;
    4:   begin Result.X := T; Result.Y := P; Result.Z := V; end;
    else begin Result.X := V; Result.Y := P; Result.Z := Q; end;
  end;
end;

function HsvToRgba(const Value: TVector3; const Alpha: Single): TCastleColor;
begin
  Result := Vector4(HsvToRgb(Value), Alpha);
end;

function RgbToHsv(const Value: TVector3Byte): TVector3;
var
  ValueFloat: TVector3;
begin
  ValueFloat.X := Value.X / 255.0;
  ValueFloat.Y := Value.Y / 255.0;
  ValueFloat.Z := Value.Z / 255.0;
  Result := RgbToHsv(ValueFloat);
end;

function HsvToRgbByte(const Value: TVector3): TVector3Byte;
var
  ResultFloat: TVector3;
begin
  ResultFloat := HsvToRgb(Value);
  Result.X := RoundClamp255(ResultFloat.X * 255.0);
  Result.Y := RoundClamp255(ResultFloat.Y * 255.0);
  Result.Z := RoundClamp255(ResultFloat.Z * 255.0);
end;

function LerpRgbInHsv(const A: Single; const V1, V2: TVector3): TVector3;
var
  H1, H2, HOut: TVector3;
  HueDiff: Single;
begin
  H1 := RgbToHsv(V1);
  H2 := RgbToHsv(V2);

  { if one of the colors has saturation = 0, then resulting hue is copied
    from the other color, not interpolated. Otherwise,
    colors with saturation = 0 get hue = 0, which causes
    interpolation from something colorful (like blue) to black go through
    weird hue. }
  if H1.Y = 0 then
    HOut.X := H2.X else
  if H2.Y = 0 then
    HOut.X := H1.X else
  begin
    HueDiff := H2.X - H1.X;
    if HueDiff > 3 then
    begin
      { from hue 1 to hue 2 go down through 0.0 }
      H2.X := H2.X - 6;
      HOut.X := H1.X + A * (H2.X - H1.X);
      if HOut.X < 0 then HOut.X := HOut.X + 6;
    end else
    if HueDiff < -3 then
    begin
      { from hue 1 to hue 2 go up through 6.0 }
      H2.X := H2.X + 6;
      HOut.X := H1.X + A * (H2.X - H1.X);
      if HOut.X > 6 then HOut.X := HOut.X - 6;
    end else
      { normal lerp when HueDiff inside [-3, 3] }
      HOut.X := H1.X + A * (H2.X - H1.X);
  end;

  { lerp on saturation and value is normal }
  HOut.Y := H1.Y + A * (H2.Y - H1.Y);
  HOut.Z := H1.Z + A * (H2.Z - H1.Z);

  Result := HsvToRgb(HOut);
end;

function ColorToHex(const V: TCastleColor): string;
var
  A: Byte;
begin
  Result := IntToHex(RoundClamp255(V.X * 255), 2) +
            IntToHex(RoundClamp255(V.Y * 255), 2) +
            IntToHex(RoundClamp255(V.Z * 255), 2);
  A := RoundClamp255(V.W * 255);
  if A <> 255 then
    Result := Result + IntToHex(A, 2);
end;

function ColorRGBToHex(const V: TCastleColorRGB): string;
begin
  Result := IntToHex(RoundClamp255(V.X * 255), 2) +
            IntToHex(RoundClamp255(V.Y * 255), 2) +
            IntToHex(RoundClamp255(V.Z * 255), 2);
end;

function HexToColor(const S: string): TCastleColor;
begin
  if Length(S) = 8 then
    Result := Vector4(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255,
      StrHexToInt(Copy(S, 7, 2)) / 255) else
  if Length(S) = 6 then
    Result := Vector4(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255,
      1.0) else
    raise EConvertError.CreateFmt('Invalid color hex string: "%s"', [S]);
end;

function HexToColorRGB(const S: string): TCastleColorRGB;
begin
  if (Length(S) = 8) or
     (Length(S) = 6) then
    Result := Vector3(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255) else
    raise EConvertError.CreateFmt('Invalid color hex string: "%s"', [S]);
end;

function ColorOpacity(const Color: TCastleColor; const Opacity: Single): TCastleColor;
begin
  Result := Color;
  Result.W := Opacity;
end;

function FadeDarkColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  { We assume that MinScale is small enough that difference between
    "Color * MinScale * screen color" and
    "MinScale * screen color" is not noticeable. }
  MinScale = 0.1;
begin
  if FadeIntensity > 0 then
  begin
    { for FadeIntensity in 1...FullWhiteEnd (going down):
      screen color := Color * original screen color }
    if FadeIntensity > FullWhiteEnd then
      Result := Color else
    { for FadeIntensity in FullWhiteEnd...FullBlack (going down):
      final screen color changes:
      - from screen color := Color * original screen color
      - to   screen color := Color * MinScale * original screen color }
    if FadeIntensity > FullBlack then
      Result := Color * MapRange(FadeIntensity, FullWhiteEnd, FullBlack, 1, MinScale) else
    { for FadeIntensity in FullBlack...0 (going down):
      final screen color changes:
      - from screen color := MinScale * original screen color
      - to   screen color := original screen color }
      Result := White * MapRange(FadeIntensity, FullBlack, 0, MinScale, 1);

    Result.W := 1.0; { alpha always 1.0 in this case }
  end else
    Result := TVector4.Zero;
end;

function FadeColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
const
  FullTime = 0.9;
var
  Intensity: Single;
begin
  if FadeIntensity > 0 then
  begin
    if FadeIntensity < FullTime then
      Intensity := MapRange(FadeIntensity, 0, FullTime, 0, 1) else
      Intensity := MapRange(FadeIntensity, FullTime, 1, 1, 0);
    Result := Color;
    Result.W := Intensity;
  end else
    Result := TVector4.Zero;
end;

end.
