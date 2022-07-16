{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit manages information about all image data formats and contains
  low level format conversion, manipulation, and other related functions.}
unit ImagingFormats;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingUtility;

type
  TImageFormatInfoArray = array[TImageFormat] of PImageFormatInfo;
  PImageFormatInfoArray = ^TImageFormatInfoArray;


{ Additional image manipulation functions (usually used internally by Imaging unit) }

type
  { Color reduction operations.}
  TReduceColorsAction = (raCreateHistogram, raUpdateHistogram, raMakeColorMap,
    raMapImage);
  TReduceColorsActions = set of TReduceColorsAction;
const
  AllReduceColorsActions = [raCreateHistogram, raUpdateHistogram,
    raMakeColorMap, raMapImage];
{ Reduces the number of colors of source. Src is bits of source image
  (ARGB or floating point) and Dst is in some indexed format. MaxColors
  is the number of colors to which reduce and DstPal is palette to which
  the resulting colors are written and it must be allocated to at least
  MaxColors entries. ChannelMask is 'anded' with every pixel's channel value
  when creating color histogram. If $FF is used all 8bits of color channels
  are used which can be slow for large images with many colors so you can
  use  lower masks to speed it up.}
procedure ReduceColorsMedianCut(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; MaxColors: LongInt; ChannelMask: Byte;
  DstPal: PPalette32; Actions: TReduceColorsActions = AllReduceColorsActions);
{ Stretches rectangle in source image to rectangle in destination image
  using nearest neighbor filtering. It is fast but results look blocky
  because there is no interpolation used. SrcImage and DstImage must be
  in the same data format. Works for all data formats except special formats.}
procedure StretchNearest(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt);
type
  { Built-in sampling filters.}
  TSamplingFilter = (sfNearest, sfLinear, sfCosine, sfHermite, sfQuadratic,
    sfGaussian, sfSpline, sfLanczos, sfMitchell, sfCatmullRom);
  { Type of custom sampling function}
  TFilterFunction = function(Value: Single): Single;
const
  { Default resampling filter used for bicubic resizing.}
  DefaultCubicFilter = sfCatmullRom;
var
  { Built-in filter functions.}
  SamplingFilterFunctions: array[TSamplingFilter] of TFilterFunction;
  { Default radii of built-in filter functions.}
  SamplingFilterRadii: array[TSamplingFilter] of Single;

{ Stretches rectangle in source image to rectangle in destination image
  with resampling. One of built-in resampling filters defined by
  Filter is used. Set WrapEdges to True for seamlessly tileable images.
  SrcImage and DstImage must be in the same data format.
  Works for all data formats except special and indexed formats.}
procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TSamplingFilter; WrapEdges: Boolean = False); overload;
{ Stretches rectangle in source image to rectangle in destination image
  with resampling. You can use custom sampling function and filter radius.
  Set WrapEdges to True for seamlessly tileable images. SrcImage and DstImage
  must be in the same data format.
  Works for all data formats except special and indexed formats.}
procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TFilterFunction; Radius: Single;
  WrapEdges: Boolean = False); overload;
{ Helper for functions that create mipmap levels. BiggerLevel is
  valid image and SmallerLevel is empty zeroed image. SmallerLevel is created
  with Width and Height dimensions and it is filled with pixels of BiggerLevel
  using resampling filter specified by ImagingMipMapFilter option.
  Uses StretchNearest and StretchResample internally so the same image data format
  limitations apply.}
procedure FillMipMapLevel(const BiggerLevel: TImageData; Width, Height: LongInt;
  var SmallerLevel: TImageData);


{ Various helper & support functions }

{ Copies Src pixel to Dest pixel. It is faster than System.Move procedure.}
procedure CopyPixel(Src, Dest: Pointer; BytesPerPixel: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Compares Src pixel and Dest pixel. It is faster than SysUtils.CompareMem function.}
function ComparePixels(PixelA, PixelB: Pointer; BytesPerPixel: LongInt): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Translates pixel color in SrcFormat to DstFormat.}
procedure TranslatePixel(SrcPixel, DstPixel: Pointer; SrcFormat,
  DstFormat: TImageFormat; SrcPalette, DstPalette: PPalette32);
{ Clamps floating point pixel channel values to [0.0, 1.0] range.}
procedure ClampFloatPixel(var PixF: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Helper function that converts pixel in any format to 32bit ARGB pixel.
  For common formats it's faster than calling GetPixel32 etc.}
procedure ConvertToPixel32(SrcPix: PByte; DestPix: PColor32Rec;
  const SrcInfo: TImageFormatInfo; SrcPalette: PPalette32 = nil); {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Adds padding bytes at the ends of scanlines. Bpp is the number of bytes per
  pixel of source and WidthBytes is the number of bytes per scanlines of dest.}
procedure AddPadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);
{ Removes padding from image with scanlines that have aligned sizes. Bpp is
  the number of bytes per pixel of dest and WidthBytes is the number of bytes
  per scanlines of source.}
procedure RemovePadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);

{ Converts 1bit image data to 8bit. Used mostly by file loaders for formats
  supporting 1bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert1To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
{ Converts 2bit image data to 8bit. Used mostly by file loaders for formats
  supporting 2bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert2To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
{ Converts 4bit image data to 8bit. Used mostly by file loaders for formats
  supporting 4bit images. Scaling of pixel values to 8bits is optional
  (indexed formats don't need this).}
procedure Convert4To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);

{ Helper function for image file loaders. Some 15 bit images (targas, bitmaps)
  may contain 1 bit alpha but there is no indication of it. This function checks
  all 16 bit(should be X1R5G5B5 or A1R5G5B5 format) pixels and some of them have
  alpha bit set it returns True, otherwise False.}
function Has16BitImageAlpha(NumPixels: LongInt; Data: PWord): Boolean;
{ Helper function for image file loaders. This function checks is similar
  to Has16BitImageAlpha but works with A8R8G8B8/X8R8G8B8 format.}
function Has32BitImageAlpha(NumPixels: LongInt; Data: PUInt32): Boolean;
{ Checks if there is any relevant alpha data (any entry has alpha <> 255)
  in the given palette.}
function PaletteHasAlpha(Palette: PPalette32; PaletteEntries: Integer): Boolean;
{ Checks if given palette has only grayscale entries.}
function PaletteIsGrayScale(Palette: PPalette32; PaletteEntries: Integer): Boolean;

{ Provides indexed access to each line of pixels. Does not work with special
  format images.}
function GetScanLine(ImageBits: Pointer; const FormatInfo: TImageFormatInfo;
  LineWidth, Index: LongInt): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns True if Format is valid image data format identifier.}
function IsImageFormatValid(Format: TImageFormat): Boolean;

{ Converts 16bit half floating point value to 32bit Single.}
function HalfToFloat(Half: THalfFloat): Single;
{ Converts 32bit Single to 16bit half floating point.}
function FloatToHalf(Float: Single): THalfFloat;

{ Converts half float color value to single-precision floating point color.}
function ColorHalfToFloat(ColorHF: TColorHFRec): TColorFPRec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Converts single-precision floating point color to half float color.}
function ColorFloatToHalf(ColorFP: TColorFPRec): TColorHFRec; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Converts ARGB color to grayscale. }
function Color32ToGray(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Makes image PalEntries x 1 big where each pixel has color of one pal entry.}
procedure VisualizePalette(Pal: PPalette32; Entries: Integer; out PalImage: TImageData);

type
  TPointRec = record
    Pos: LongInt;
    Weight: Single;
  end;
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

{ Helper function for resampling.}
function BuildMappingTable(DstLow, DstHigh, SrcLow, SrcHigh, SrcImageWidth: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean): TMappingTable;
{ Helper function for resampling.}
procedure FindExtremes(const Map: TMappingTable; var MinPos, MaxPos: LongInt);


{ Pixel readers/writers for different image formats }

{ Returns pixel of image in any ARGB format. Channel values are scaled to 16 bits.}
procedure ChannelGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColor64Rec);
{ Sets pixel of image in any ARGB format. Channel values must be scaled to 16 bits.}
procedure ChannelSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColor64Rec);

{ Returns pixel of image in any grayscale format. Gray value is scaled to 64 bits
  and alpha to 16 bits.}
procedure GrayGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Gray: TColor64Rec; var Alpha: Word);
{ Sets pixel of image in any grayscale format. Gray value must be scaled to 64 bits
  and alpha to 16 bits.}
procedure GraySetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Gray: TColor64Rec; Alpha: Word);

{ Returns pixel of image in any floating point format. Channel values are
  in range <0.0, 1.0>.}
procedure FloatGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColorFPRec);
{ Sets pixel of image in any floating point format. Channel values must be
  in range <0.0, 1.0>.}
procedure FloatSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColorFPRec);

{ Returns pixel of image in any indexed format. Returned value is index to
  the palette.}
procedure IndexGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Index: UInt32);
{ Sets pixel of image in any indexed format. Index is index to the palette.}
procedure IndexSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  Index: UInt32);


{ Pixel readers/writers for 32bit and FP colors}

{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to 32 bit ARGB.}
function GetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32): TColor32Rec;
{ Procedure for setting pixel colors. Input 32 bit ARGB color is translated to
    native format and then written to Image.}
procedure SetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32; const Color: TColor32Rec);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to FP ARGB.}
function GetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32): TColorFPRec;
{ Procedure for setting pixel colors. Input FP ARGB color is translated to
    native format and then written to Image.}
procedure SetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo;
  Palette: PPalette32; const Color: TColorFPRec);


{ Image format conversion functions }

{ Converts any ARGB format to any ARGB format.}
procedure ChannelToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any grayscale format.}
procedure ChannelToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any floating point format.}
procedure ChannelToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any ARGB format to any indexed format.}
procedure ChannelToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any grayscale format to any grayscale format.}
procedure GrayToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any ARGB format.}
procedure GrayToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any floating point format.}
procedure GrayToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any grayscale format to any indexed format.}
procedure GrayToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any floating point format to any floating point format.}
procedure FloatToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any ARGB format.}
procedure FloatToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any grayscale format.}
procedure FloatToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
{ Converts any floating point format to any indexed format.}
procedure FloatToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);

{ Converts any indexed format to any indexed format.}
procedure IndexToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal, DstPal: PPalette32);
{ Converts any indexed format to any ARGB format.}
procedure IndexToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
{ Converts any indexed format to any grayscale format.}
procedure IndexToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
{ Converts any indexed format to any floating point  format.}
procedure IndexToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);

{ Special formats conversion functions }

{ Converts image to/from/between special image formats (dxtc, ...).}
procedure ConvertSpecial(var Image: TImageData; SrcInfo,
  DstInfo: PImageFormatInfo);


{ Inits all image format information. Called internally on startup.}
procedure InitImageFormats(var Infos: TImageFormatInfoArray);

const
  // Grayscale conversion channel weights
  GrayConv: TColorFPRec = (B: 0.114; G: 0.587; R: 0.299; A: 0.0);

  // Constants for converting integer colors to floating point
  OneDiv8Bit: Single = 1.0 / 255.0;
  OneDiv16Bit: Single = 1.0 / 65535.0;

implementation

{ TImageFormatInfo member functions }

{ Returns size in bytes of image in given standard format where
  Size = Width * Height * Bpp.}
function GetStdPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; forward;
{ Checks if Width and Height are valid for given standard format.}
procedure CheckStdDimensions(Format: TImageFormat; var Width, Height: LongInt); forward;
{ Returns size in bytes of image in given DXT format.}
function GetDXTPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; forward;
{ Checks if Width and Height are valid for given DXT format. If they are
  not valid, they are changed to pass the check.}
procedure CheckDXTDimensions(Format: TImageFormat; var Width, Height: LongInt); forward;
{ Returns size in bytes of image in BTC format.}
function GetBTCPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; forward;
{ Returns size in bytes of image in binary format (1bit image).}
function GetBinaryPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; forward;

function GetBCPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; forward;
procedure CheckBCDimensions(Format: TImageFormat; var Width, Height: LongInt); forward;


{ Optimized pixel readers/writers for 32bit and FP colors to be stored in TImageFormatInfo }

function GetPixel32ifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColor32Rec; forward;
procedure SetPixel32ifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColor32Rec); forward;
function GetPixelFPifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec; forward;
procedure SetPixelFPifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec); forward;

function GetPixel32Channel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColor32Rec; forward;
procedure SetPixel32Channel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColor32Rec); forward;
function GetPixelFPChannel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec; forward;
procedure SetPixelFPChannel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec); forward;

function GetPixelFPFloat32(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec; forward;
procedure SetPixelFPFloat32(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec); forward;

var
  PFR3G3B2: TPixelFormatInfo;
  PFX5R1G1B1: TPixelFormatInfo;
  PFR5G6B5: TPixelFormatInfo;
  PFA1R5G5B5: TPixelFormatInfo;
  PFA4R4G4B4: TPixelFormatInfo;
  PFX1R5G5B5: TPixelFormatInfo;
  PFX4R4G4B4: TPixelFormatInfo;
  FInfos: PImageFormatInfoArray;

var
  // Free Pascal generates hundreds of warnings here
{$WARNINGS OFF}

  // indexed formats
  Index8Info: TImageFormatInfo = (
    Format: ifIndex8;
    Name: 'Index8';
    BytesPerPixel: 1;
    ChannelCount: 1;
    PaletteEntries: 256;
    HasAlphaChannel: True;
    IsIndexed: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  // grayscale formats
  Gray8Info: TImageFormatInfo = (
    Format: ifGray8;
    Name: 'Gray8';
    BytesPerPixel: 1;
    ChannelCount: 1;
    HasGrayChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Channel8Bit;
    GetPixelFP: GetPixelFPChannel8Bit;
    SetPixel32: SetPixel32Channel8Bit;
    SetPixelFP: SetPixelFPChannel8Bit);

  A8Gray8Info: TImageFormatInfo = (
    Format: ifA8Gray8;
    Name: 'A8Gray8';
    BytesPerPixel: 2;
    ChannelCount: 2;
    HasGrayChannel: True;
    HasAlphaChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Channel8Bit;
    GetPixelFP: GetPixelFPChannel8Bit;
    SetPixel32: SetPixel32Channel8Bit;
    SetPixelFP: SetPixelFPChannel8Bit);

  Gray16Info: TImageFormatInfo = (
    Format: ifGray16;
    Name: 'Gray16';
    BytesPerPixel: 2;
    ChannelCount: 1;
    HasGrayChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  Gray32Info: TImageFormatInfo = (
    Format: ifGray32;
    Name: 'Gray32';
    BytesPerPixel: 4;
    ChannelCount: 1;
    HasGrayChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  Gray64Info: TImageFormatInfo = (
    Format: ifGray64;
    Name: 'Gray64';
    BytesPerPixel: 8;
    ChannelCount: 1;
    HasGrayChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A16Gray16Info: TImageFormatInfo = (
    Format: ifA16Gray16;
    Name: 'A16Gray16';
    BytesPerPixel: 4;
    ChannelCount: 2;
    HasGrayChannel: True;
    HasAlphaChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  // ARGB formats
  X5R1G1B1Info: TImageFormatInfo = (
    Format: ifX5R1G1B1;
    Name: 'X5R1G1B1';
    BytesPerPixel: 1;
    ChannelCount: 3;
    UsePixelFormat: True;
    PixelFormat: @PFX5R1G1B1;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  R3G3B2Info: TImageFormatInfo = (
    Format: ifR3G3B2;
    Name: 'R3G3B2';
    BytesPerPixel: 1;
    ChannelCount: 3;
    UsePixelFormat: True;
    PixelFormat: @PFR3G3B2;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  R5G6B5Info: TImageFormatInfo = (
    Format: ifR5G6B5;
    Name: 'R5G6B5';
    BytesPerPixel: 2;
    ChannelCount: 3;
    UsePixelFormat: True;
    PixelFormat: @PFR5G6B5;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A1R5G5B5Info: TImageFormatInfo = (
    Format: ifA1R5G5B5;
    Name: 'A1R5G5B5';
    BytesPerPixel: 2;
    ChannelCount: 4;
    HasAlphaChannel: True;
    UsePixelFormat: True;
    PixelFormat: @PFA1R5G5B5;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A4R4G4B4Info: TImageFormatInfo = (
    Format: ifA4R4G4B4;
    Name: 'A4R4G4B4';
    BytesPerPixel: 2;
    ChannelCount: 4;
    HasAlphaChannel: True;
    UsePixelFormat: True;
    PixelFormat: @PFA4R4G4B4;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  X1R5G5B5Info: TImageFormatInfo = (
    Format: ifX1R5G5B5;
    Name: 'X1R5G5B5';
    BytesPerPixel: 2;
    ChannelCount: 3;
    UsePixelFormat: True;
    PixelFormat: @PFX1R5G5B5;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  X4R4G4B4Info: TImageFormatInfo = (
    Format: ifX4R4G4B4;
    Name: 'X4R4G4B4';
    BytesPerPixel: 2;
    ChannelCount: 3;
    UsePixelFormat: True;
    PixelFormat: @PFX4R4G4B4;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  R8G8B8Info: TImageFormatInfo = (
    Format: ifR8G8B8;
    Name: 'R8G8B8';
    BytesPerPixel: 3;
    ChannelCount: 3;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Channel8Bit;
    GetPixelFP: GetPixelFPChannel8Bit;
    SetPixel32: SetPixel32Channel8Bit;
    SetPixelFP: SetPixelFPChannel8Bit);

  A8R8G8B8Info: TImageFormatInfo = (
    Format: ifA8R8G8B8;
    Name: 'A8R8G8B8';
    BytesPerPixel: 4;
    ChannelCount: 4;
    HasAlphaChannel: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32ifA8R8G8B8;
    GetPixelFP: GetPixelFPifA8R8G8B8;
    SetPixel32: SetPixel32ifA8R8G8B8;
    SetPixelFP: SetPixelFPifA8R8G8B8);

  X8R8G8B8Info: TImageFormatInfo = (
    Format: ifX8R8G8B8;
    Name: 'X8R8G8B8';
    BytesPerPixel: 4;
    ChannelCount: 3;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Channel8Bit;
    GetPixelFP: GetPixelFPChannel8Bit;
    SetPixel32: SetPixel32Channel8Bit;
    SetPixelFP: SetPixelFPChannel8Bit);

  R16G16B16Info: TImageFormatInfo = (
    Format: ifR16G16B16;
    Name: 'R16G16B16';
    BytesPerPixel: 6;
    ChannelCount: 3;
    RBSwapFormat: ifB16G16R16;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A16R16G16B16Info: TImageFormatInfo = (
    Format: ifA16R16G16B16;
    Name: 'A16R16G16B16';
    BytesPerPixel: 8;
    ChannelCount: 4;
    HasAlphaChannel: True;
    RBSwapFormat: ifA16B16G16R16;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  B16G16R16Info: TImageFormatInfo = (
    Format: ifB16G16R16;
    Name: 'B16G16R16';
    BytesPerPixel: 6;
    ChannelCount: 3;
    IsRBSwapped: True;
    RBSwapFormat: ifR16G16B16;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A16B16G16R16Info: TImageFormatInfo = (
    Format: ifA16B16G16R16;
    Name: 'A16B16G16R16';
    BytesPerPixel: 8;
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsRBSwapped: True;
    RBSwapFormat: ifA16R16G16B16;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  // floating point formats
  R32FInfo: TImageFormatInfo = (
    Format: ifR32F;
    Name: 'R32F';
    BytesPerPixel: 4;
    ChannelCount: 1;
    IsFloatingPoint: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPFloat32;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPFloat32);

 A32R32G32B32FInfo: TImageFormatInfo = (
    Format: ifA32R32G32B32F;
    Name: 'A32R32G32B32F';
    BytesPerPixel: 16;
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsFloatingPoint: True;
    RBSwapFormat: ifA32B32G32R32F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPFloat32;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPFloat32);

  A32B32G32R32FInfo: TImageFormatInfo = (
    Format: ifA32B32G32R32F;
    Name: 'A32B32G32R32F';
    BytesPerPixel: 16;
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsFloatingPoint: True;
    IsRBSwapped: True;
    RBSwapFormat: ifA32R32G32B32F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPFloat32;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPFloat32);

  R16FInfo: TImageFormatInfo = (
    Format: ifR16F;
    Name: 'R16F';
    BytesPerPixel: 2;
    ChannelCount: 1;
    IsFloatingPoint: True;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

 A16R16G16B16FInfo: TImageFormatInfo = (
    Format: ifA16R16G16B16F;
    Name: 'A16R16G16B16F';
    BytesPerPixel: 8;
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsFloatingPoint: True;
    RBSwapFormat: ifA16B16G16R16F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  A16B16G16R16FInfo: TImageFormatInfo = (
    Format: ifA16B16G16R16F;
    Name: 'A16B16G16R16F';
    BytesPerPixel: 8;
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsFloatingPoint: True;
    IsRBSwapped: True;
    RBSwapFormat: ifA16R16G16B16F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPGeneric;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPGeneric);

  R32G32B32FInfo: TImageFormatInfo = (
    Format: ifR32G32B32F;
    Name: 'R32G32B32F';
    BytesPerPixel: 12;
    ChannelCount: 3;
    IsFloatingPoint: True;
    RBSwapFormat: ifB32G32R32F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPFloat32;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPFloat32);

  B32G32R32FInfo: TImageFormatInfo = (
    Format: ifB32G32R32F;
    Name: 'B32G32R32F';
    BytesPerPixel: 12;
    ChannelCount: 3;
    IsFloatingPoint: True;
    IsRBSwapped: True;
    RBSwapFormat: ifR32G32B32F;
    GetPixelsSize: GetStdPixelsSize;
    CheckDimensions: CheckStdDimensions;
    GetPixel32: GetPixel32Generic;
    GetPixelFP: GetPixelFPFloat32;
    SetPixel32: SetPixel32Generic;
    SetPixelFP: SetPixelFPFloat32);

  // special formats
  DXT1Info: TImageFormatInfo = (
    Format: ifDXT1;
    Name: 'DXT1';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    GetPixelsSize: GetDXTPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  DXT3Info: TImageFormatInfo = (
    Format: ifDXT3;
    Name: 'DXT3';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    GetPixelsSize: GetDXTPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  DXT5Info: TImageFormatInfo = (
    Format: ifDXT5;
    Name: 'DXT5';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    GetPixelsSize: GetDXTPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  BTCInfo: TImageFormatInfo = (
    Format: ifBTC;
    Name: 'BTC';
    ChannelCount: 1;
    HasAlphaChannel: False;
    IsSpecial: True;
    GetPixelsSize: GetBTCPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifGray8);

  ATI1NInfo: TImageFormatInfo = (
    Format: ifATI1N;
    Name: 'ATI1N';
    ChannelCount: 1;
    HasAlphaChannel: False;
    IsSpecial: True;
    GetPixelsSize: GetDXTPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifGray8);

  ATI2NInfo: TImageFormatInfo = (
    Format: ifATI2N;
    Name: 'ATI2N';
    ChannelCount: 2;
    HasAlphaChannel: False;
    IsSpecial: True;
    GetPixelsSize: GetDXTPixelsSize;
    CheckDimensions: CheckDXTDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  BinaryInfo: TImageFormatInfo = (
    Format: ifBinary;
    Name: 'Binary';
    ChannelCount: 1;
    HasAlphaChannel: False;
    IsSpecial: True;
    GetPixelsSize: GetBinaryPixelsSize;
    CheckDimensions: CheckStdDimensions;
    SpecialNearestFormat: ifGray8);

  { Passtrough formats }

  {ETC1Info: TImageFormatInfo = (
    Format: ifETC1;
    Name: 'ETC1';
    ChannelCount: 3;
    HasAlphaChannel: False;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifR8G8B8);

  ETC2RGBInfo: TImageFormatInfo = (
    Format: ifETC2RGB;
    Name: 'ETC2RGB';
    ChannelCount: 3;
    HasAlphaChannel: False;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifR8G8B8);

  ETC2RGBAInfo: TImageFormatInfo = (
    Format: ifETC2RGBA;
    Name: 'ETC2RGBA';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  ETC2PAInfo: TImageFormatInfo = (
    Format: ifETC2PA;
    Name: 'ETC2PA';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  DXBC6Info: TImageFormatInfo = (
    Format: ifDXBC6;
    Name: 'DXBC6';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  DXBC7Info: TImageFormatInfo = (
    Format: ifDXBC6;
    Name: 'DXBC7';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifA8R8G8B8);

  PVRTCInfo: TImageFormatInfo = (
    Format: ifPVRTC;
    Name: 'PVRTC';
    ChannelCount: 4;
    HasAlphaChannel: True;
    IsSpecial: True;
    IsPassthrough: True;
    GetPixelsSize: GetBCPixelsSize;
    CheckDimensions: CheckBCDimensions;
    SpecialNearestFormat: ifA8R8G8B8);}

{$WARNINGS ON}

function PixelFormat(ABitCount, RBitCount, GBitCount, BBitCount: Byte): TPixelFormatInfo; forward;

procedure InitImageFormats(var Infos: TImageFormatInfoArray);
begin
  FInfos := @Infos;

  Infos[ifDefault] := @A8R8G8B8Info;
  // indexed formats
  Infos[ifIndex8] := @Index8Info;
  // grayscale formats
  Infos[ifGray8] := @Gray8Info;
  Infos[ifA8Gray8] := @A8Gray8Info;
  Infos[ifGray16] := @Gray16Info;
  Infos[ifGray32] := @Gray32Info;
  Infos[ifGray64] := @Gray64Info;
  Infos[ifA16Gray16] := @A16Gray16Info;
  // ARGB formats
  Infos[ifX5R1G1B1] := @X5R1G1B1Info;
  Infos[ifR3G3B2] := @R3G3B2Info;
  Infos[ifR5G6B5] := @R5G6B5Info;
  Infos[ifA1R5G5B5] := @A1R5G5B5Info;
  Infos[ifA4R4G4B4] := @A4R4G4B4Info;
  Infos[ifX1R5G5B5] := @X1R5G5B5Info;
  Infos[ifX4R4G4B4] := @X4R4G4B4Info;
  Infos[ifR8G8B8] := @R8G8B8Info;
  Infos[ifA8R8G8B8] := @A8R8G8B8Info;
  Infos[ifX8R8G8B8] := @X8R8G8B8Info;
  Infos[ifR16G16B16] := @R16G16B16Info;
  Infos[ifA16R16G16B16] := @A16R16G16B16Info;
  Infos[ifB16G16R16] := @B16G16R16Info;
  Infos[ifA16B16G16R16] := @A16B16G16R16Info;
  // floating point formats
  Infos[ifR32F] := @R32FInfo;
  Infos[ifA32R32G32B32F] := @A32R32G32B32FInfo;
  Infos[ifA32B32G32R32F] := @A32B32G32R32FInfo;
  Infos[ifR16F] := @R16FInfo;
  Infos[ifA16R16G16B16F] := @A16R16G16B16FInfo;
  Infos[ifA16B16G16R16F] := @A16B16G16R16FInfo;
  Infos[ifR32G32B32F] := @R32G32B32FInfo;
  Infos[ifB32G32R32F] := @B32G32R32FInfo;
  // special formats
  Infos[ifDXT1] := @DXT1Info;
  Infos[ifDXT3] := @DXT3Info;
  Infos[ifDXT5] := @DXT5Info;
  Infos[ifBTC] :=  @BTCInfo;
  Infos[ifATI1N] := @ATI1NInfo;
  Infos[ifATI2N] := @ATI2NInfo;
  Infos[ifBinary] := @BinaryInfo;

  PFR3G3B2 := PixelFormat(0, 3, 3, 2);
  PFX5R1G1B1 := PixelFormat(0, 1, 1, 1);
  PFR5G6B5 := PixelFormat(0, 5, 6, 5);
  PFA1R5G5B5 := PixelFormat(1, 5, 5, 5);
  PFA4R4G4B4 := PixelFormat(4, 4, 4, 4);
  PFX1R5G5B5 := PixelFormat(0, 5, 5, 5);
  PFX4R4G4B4 := PixelFormat(0, 4, 4, 4);
end;


{ Internal unit helper functions }

function PixelFormat(ABitCount, RBitCount, GBitCount, BBitCount: Byte): TPixelFormatInfo;
begin
  Result.ABitMask := ((1 shl ABitCount) - 1) shl (RBitCount + GBitCount +
    BBitCount);
  Result.RBitMask := ((1 shl RBitCount) - 1) shl (GBitCount + BBitCount);
  Result.GBitMask := ((1 shl GBitCount) - 1) shl (BBitCount);
  Result.BBitMask := (1 shl BBitCount) - 1;
  Result.ABitCount := ABitCount;
  Result.RBitCount := RBitCount;
  Result.GBitCount := GBitCount;
  Result.BBitCount := BBitCount;
  Result.AShift := RBitCount + GBitCount + BBitCount;
  Result.RShift := GBitCount + BBitCount;
  Result.GShift := BBitCount;
  Result.BShift := 0;
  Result.ARecDiv := Max(1, Pow2Int(Result.ABitCount) - 1);
  Result.RRecDiv := Max(1, Pow2Int(Result.RBitCount) - 1);
  Result.GRecDiv := Max(1, Pow2Int(Result.GBitCount) - 1);
  Result.BRecDiv := Max(1, Pow2Int(Result.BBitCount) - 1);
end;

function PixelFormatMask(ABitMask, RBitMask, GBitMask, BBitMask: UInt32): TPixelFormatInfo;

  function GetBitCount(B: UInt32): UInt32;
  var
    I: UInt32;
  begin
    I := 0;
    while (I < 31) and (((1 shl I) and B) = 0) do
      Inc(I);
    Result := 0;
    while ((1 shl I) and B) <> 0 do
    begin
      Inc(I);
      Inc(Result);
    end;
  end;

begin
  Result := PixelFormat(GetBitCount(ABitMask), GetBitCount(RBitMask),
    GetBitCount(GBitMask), GetBitCount(BBitMask));
end;

function PFSetARGB(const PF: TPixelFormatInfo; A, R, G, B: Byte): TColor32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  with PF do
    Result :=
      (A shl ABitCount shr 8 shl AShift) or
      (R shl RBitCount shr 8 shl RShift) or
      (G shl GBitCount shr 8 shl GShift) or
      (B shl BBitCount shr 8 shl BShift);
end;

procedure PFGetARGB(const PF: TPixelFormatInfo; Color: UInt32;
  var A, R, G, B: Byte); {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  with PF do
  begin
    A := (Color and ABitMask shr AShift) * 255 div ARecDiv;
    R := (Color and RBitMask shr RShift) * 255 div RRecDiv;
    G := (Color and GBitMask shr GShift) * 255 div GRecDiv;
    B := (Color and BBitMask shl BShift) * 255 div BRecDiv;
  end;
end;

function PFSetColor(const PF: TPixelFormatInfo; ARGB: TColor32): UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  with PF do
    Result :=
      (Byte(ARGB shr 24) shl ABitCount shr 8 shl AShift) or
      (Byte(ARGB shr 16) shl RBitCount shr 8 shl RShift) or
      (Byte(ARGB shr 8) shl GBitCount shr 8 shl GShift) or
      (Byte(ARGB) shl BBitCount shr 8 shl BShift);
end;

// Castle Game Engine added:
{$ifdef FPC} {$push} {$warnings off} {$endif} // avoid Warning: Function result variable does not seem to be initialized
function PFGetColor(const PF: TPixelFormatInfo; Color: UInt32): TColor32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  with PF, TColor32Rec(Result) do
  begin
    A := (Color and ABitMask shr AShift) * 255 div ARecDiv;
    R := (Color and RBitMask shr RShift) * 255 div RRecDiv;
    G := (Color and GBitMask shr GShift) * 255 div GRecDiv;
    B := (Color and BBitMask shl BShift) * 255 div BRecDiv;
  end;
end;
{$ifdef FPC} {$pop} {$endif}

{ Additional image manipulation functions (usually used internally by Imaging unit) }

const
  MaxPossibleColors = 4096;
  HashSize = 32768;
  AlphaWeight = 1024;
  RedWeight = 612;
  GreenWeight = 1202;
  BlueWeight = 234;

type
  PColorBin = ^TColorBin;
  TColorBin = record
    Color: TColor32Rec;
    Number: LongInt;
    Next: PColorBin;
  end;

  THashTable = array[0..HashSize - 1] of PColorBin;

  TColorBox = record
    AMin, AMax,
    RMin, RMax,
    GMin, GMax,
    BMin, BMax: LongInt;
    Total: LongInt;
    Represented: TColor32Rec;
    List: PColorBin;
  end;

var
  Table: THashTable;
  Box: array[0..MaxPossibleColors - 1] of TColorBox;
  Boxes: LongInt;

procedure ReduceColorsMedianCut(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; MaxColors: LongInt; ChannelMask: Byte;
  DstPal: PPalette32; Actions: TReduceColorsActions);

  procedure CreateHistogram (Src: PByte; SrcInfo: PImageFormatInfo;
    ChannelMask: Byte);
  var
    A, R, G, B: Byte;
    I, Addr: LongInt;
    PC: PColorBin;
    Col: TColor32Rec;
  begin
    for I := 0 to NumPixels - 1 do
    begin
      Col := GetPixel32Generic(Src, SrcInfo, nil);
      A := Col.A and ChannelMask;
      R := Col.R and ChannelMask;
      G := Col.G and ChannelMask;
      B := Col.B and ChannelMask;

      Addr := (A + 11 * B + 59 * R + 119 * G) mod HashSize;
      PC := Table[Addr];

      while (PC <> nil) and ((PC.Color.R <> R) or (PC.Color.G <> G) or
        (PC.Color.B <> B) or (PC.Color.A <> A)) do
        PC := PC.Next;

      if PC = nil then
      begin
        New(PC);
        PC.Color.R := R;
        PC.Color.G := G;
        PC.Color.B := B;
        PC.Color.A := A;
        PC.Number := 1;
        PC.Next := Table[Addr];
        Table[Addr] := PC;
      end
      else
        Inc(PC^.Number);
      Inc(Src, SrcInfo.BytesPerPixel);
    end;
  end;

  procedure InitBox (var Box : TColorBox);
  begin
    Box.AMin := 256;
    Box.RMin := 256;
    Box.GMin := 256;
    Box.BMin := 256;
    Box.AMax := -1;
    Box.RMax := -1;
    Box.GMax := -1;
    Box.BMax := -1;
    Box.Total := 0;
    Box.List := nil;
  end;

  procedure ChangeBox (var Box: TColorBox; const C: TColorBin);
  begin
    with C.Color do
    begin
      if A < Box.AMin then Box.AMin := A;
      if A > Box.AMax then Box.AMax := A;
      if B < Box.BMin then Box.BMin := B;
      if B > Box.BMax then Box.BMax := B;
      if G < Box.GMin then Box.GMin := G;
      if G > Box.GMax then Box.GMax := G;
      if R < Box.RMin then Box.RMin := R;
      if R > Box.RMax then Box.RMax := R;
    end;
    Inc(Box.Total, C.Number);
  end;

  procedure MakeColorMap;
  var
    I, J: LongInt;
    CP, Pom: PColorBin;
    Cut, LargestIdx, Largest, Size, S: LongInt;
    CutA, CutR, CutG, CutB: Boolean;
    SumA, SumR, SumG, SumB: LongInt;
    Temp: TColorBox;
  begin
    I := 0;
    Boxes := 1;
    LargestIdx := 0;
    while (I < HashSize) and (Table[I] = nil) do
      Inc(i);
    if I < HashSize then
    begin
      // put all colors into Box[0]
      InitBox(Box[0]);
      repeat
        CP := Table[I];
        while CP.Next <> nil do
        begin
          ChangeBox(Box[0], CP^);
          CP := CP.Next;
        end;
        ChangeBox(Box[0], CP^);
        CP.Next := Box[0].List;
        Box[0].List := Table[I];
        Table[I] := nil;
        repeat
          Inc(I)
        until (I = HashSize) or (Table[I] <> nil);
      until I = HashSize;
      // now all colors are in Box[0]
      repeat
        // cut one color box
        Largest := 0;
        for I := 0 to Boxes - 1 do
          with Box[I] do
          begin
            Size := (AMax - AMin) * AlphaWeight;
            S := (RMax - RMin) * RedWeight;
            if S > Size then
              Size := S;
            S := (GMax - GMin) * GreenWeight;
            if S > Size then
              Size := S;
            S := (BMax - BMin) * BlueWeight;
            if S > Size then
              Size := S;
            if Size > Largest then
            begin
              Largest := Size;
              LargestIdx := I;
            end;
          end;
        if Largest > 0 then
        begin
          // cutting Box[LargestIdx] into Box[LargestIdx] and Box[Boxes]
          CutR := False;
          CutG := False;
          CutB := False;
          CutA := False;
          with Box[LargestIdx] do
          begin
            if (AMax - AMin) * AlphaWeight = Largest then
            begin
              Cut := (AMax + AMin) shr 1;
              CutA := True;
            end
            else
              if (RMax - RMin) * RedWeight = Largest then
              begin
                Cut := (RMax + RMin) shr 1;
                CutR := True;
              end
              else
                if (GMax - GMin) * GreenWeight = Largest then
                begin
                  Cut := (GMax + GMin) shr 1;
                  CutG := True;
                end
                else
                begin
                  Cut := (BMax + BMin) shr 1;
                  CutB := True;
                end;
            CP := List;
          end;
          InitBox(Box[LargestIdx]);
          InitBox(Box[Boxes]);
          repeat
            // distribute one color
            Pom := CP.Next;
            with CP.Color do
            begin
              if (CutA and (A <= Cut)) or (CutR and (R <= Cut)) or
                (CutG and (G <= Cut)) or (CutB and (B <= Cut)) then
                I := LargestIdx
              else
                I := Boxes;
            end;
            CP.Next := Box[i].List;
            Box[i].List := CP;
            ChangeBox(Box[i], CP^);
            CP := Pom;
          until CP = nil;
          Inc(Boxes);
        end;
      until (Boxes = MaxColors) or (Largest = 0);
      // compute box representation
      for I := 0 to Boxes - 1 do
      begin
        SumR := 0;
        SumG := 0;
        SumB := 0;
        SumA := 0;
        repeat
          CP := Box[I].List;
          Inc(SumR, CP.Color.R * CP.Number);
          Inc(SumG, CP.Color.G * CP.Number);
          Inc(SumB, CP.Color.B * CP.Number);
          Inc(SumA, CP.Color.A * CP.Number);
          Box[I].List := CP.Next;
          Dispose(CP);
        until Box[I].List = nil;
        with Box[I] do
        begin
          Represented.A := SumA div Total;
          Represented.R := SumR div Total;
          Represented.G := SumG div Total;
          Represented.B := SumB div Total;
          AMin := AMin and ChannelMask;
          RMin := RMin and ChannelMask;
          GMin := GMin and ChannelMask;
          BMin := BMin and ChannelMask;
          AMax := (AMax and ChannelMask) + (not ChannelMask);
          RMax := (RMax and ChannelMask) + (not ChannelMask);
          GMax := (GMax and ChannelMask) + (not ChannelMask);
          BMax := (BMax and ChannelMask) + (not ChannelMask);
        end;
      end;
      // sort color boxes
      for I := 0 to Boxes - 2 do
      begin
        Largest := 0;
        for J := I to Boxes - 1 do
          if Box[J].Total > Largest then
          begin
            Largest := Box[J].Total;
            LargestIdx := J;
          end;
        if LargestIdx <> I then
        begin
          Temp := Box[I];
          Box[I] := Box[LargestIdx];
          Box[LargestIdx] := Temp;
        end;
      end;
    end;
  end;

  procedure FillOutputPalette;
  var
    I: LongInt;
  begin
    FillChar(DstPal^, SizeOf(TColor32Rec) * MaxColors, $FF);
    for I := 0 to MaxColors - 1 do
    begin
      if I < Boxes then
      with Box[I].Represented do
      begin
        DstPal[I].A := A;
        DstPal[I].R := R;
        DstPal[I].G := G;
        DstPal[I].B := B;
      end
      else
        DstPal[I].Color := $FF000000;
    end;
  end;

  function MapColor(const Col: TColor32Rec) : LongInt;
  var
    I: LongInt;
  begin
    I := 0;
    with Col do
      while (I < Boxes) and ((Box[I].AMin > A) or (Box[I].AMax < A) or
        (Box[I].RMin > R) or (Box[I].RMax < R) or (Box[I].GMin > G) or
        (Box[I].GMax < G) or (Box[I].BMin > B) or (Box[I].BMax < B)) do
        Inc(I);
    if I = Boxes then
      MapColor := 0
    else
      MapColor := I;
  end;

  procedure MapImage(Src, Dst: PByte; SrcInfo, DstInfo: PImageFormatInfo);
  var
    I: LongInt;
    Col: TColor32Rec;
  begin
    for I := 0 to NumPixels - 1 do
    begin
      Col := GetPixel32Generic(Src, SrcInfo, nil);
      IndexSetDstPixel(Dst, DstInfo, MapColor(Col));
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
  end;

begin
  MaxColors := ClampInt(MaxColors, 2, MaxPossibleColors);

  if (raUpdateHistogram in Actions) or (raMapImage in Actions) then
  begin
    Assert(not SrcInfo.IsSpecial);
    Assert(not SrcInfo.IsIndexed);
  end;

  if raCreateHistogram in Actions then
    FillChar(Table, SizeOf(Table), 0);

  if raUpdateHistogram in Actions then
    CreateHistogram(Src, SrcInfo, ChannelMask);

  if raMakeColorMap in Actions then
  begin
    MakeColorMap;
    FillOutputPalette;
  end;

  if raMapImage in Actions then
    MapImage(Src, Dst, SrcInfo, DstInfo);
end;

procedure StretchNearest(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt);
var
  Info: TImageFormatInfo;
  ScaleX, ScaleY, X, Y, Xp, Yp: LongInt;
  DstPixel, SrcLine: PByte;
begin
  GetImageFormatInfo(SrcImage.Format, Info);
  Assert(SrcImage.Format = DstImage.Format);
  Assert(not Info.IsSpecial);
  // Use integers instead of floats for source image pixel coords
  // Xp and Yp coords must be shifted right to get read source image coords
  ScaleX := (SrcWidth shl 16) div DstWidth;
  ScaleY := (SrcHeight shl 16) div DstHeight;
  Yp := 0;
  for Y := 0 to DstHeight - 1 do
  begin
    Xp := 0;
    SrcLine := @PByteArray(SrcImage.Bits)[((SrcY + Yp shr 16) * SrcImage.Width + SrcX) * Info.BytesPerPixel];
    DstPixel := @PByteArray(DstImage.Bits)[((DstY + Y) * DstImage.Width + DstX) * Info.BytesPerPixel];
    for X := 0 to DstWidth - 1 do
    begin
      case Info.BytesPerPixel of
        1: PByte(DstPixel)^ := PByteArray(SrcLine)[Xp shr 16];
        2: PWord(DstPixel)^ := PWordArray(SrcLine)[Xp shr 16];
        3: PColor24Rec(DstPixel)^ := PPalette24(SrcLine)[Xp shr 16];
        4: PColor32(DstPixel)^ := PUInt32Array(SrcLine)[Xp shr 16];
        6: PColor48Rec(DstPixel)^ := PColor48RecArray(SrcLine)[Xp shr 16];
        8: PColor64(DstPixel)^ := PInt64Array(SrcLine)[Xp shr 16];
        16: PColorFPRec(DstPixel)^ := PColorFPRecArray(SrcLine)[Xp shr 16];
      end;
      Inc(DstPixel, Info.BytesPerPixel);
      Inc(Xp, ScaleX);
    end;
    Inc(Yp, ScaleY);
  end;
end;

{ Filter function for nearest filtering. Also known as box filter.}
function FilterNearest(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

{ Filter function for linear filtering. Also known as triangle or Bartlett filter.}
function FilterLinear(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

{ Cosine filter.}
function FilterCosine(Value: Single): Single;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) / 2;
end;

{ f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1 }
function FilterHermite(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

{ Quadratic filter. Also known as Bell.}
function FilterQuadratic(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
  if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

{ Gaussian filter.}
function FilterGaussian(Value: Single): Single;
begin
  Result := Exp(-2.0 * Sqr(Value)) * Sqrt(2.0 / Pi);
end;

{ 4th order (cubic) b-spline filter.}
function FilterSpline(Value: Single): Single;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2.0 / 3.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := Sqr(Value) * Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ Lanczos-windowed sinc filter.}
function FilterLanczos(Value: Single): Single;

  function SinC(Value: Single): Single;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else
      Result := 1.0;
  end;

begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 3.0 then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

{ Mitchell cubic filter.}
function FilterMitchell(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * Temp)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * Temp) +
      (6.0 - 2.0 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-B - 6.0 * C) * (Value * Temp)) +
      ((6.0 * B + 30.0 * C) * Temp) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24.0 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ CatmullRom spline filter.}
function FilterCatmullRom(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 0.5 * (2.0 + Sqr(Value) * (-5.0 + 3.0 * Value))
  else
  if Value < 2.0 then
    Result := 0.5 * (4.0 + Value * (-8.0 + Value * (5.0 - Value)))
  else
    Result := 0.0;
end;

procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TSamplingFilter; WrapEdges: Boolean);
begin
  // Calls the other function with filter function and radius defined by Filter
  StretchResample(SrcImage, SrcX, SrcY, SrcWidth, SrcHeight, DstImage, DstX, DstY,
    DstWidth, DstHeight, SamplingFilterFunctions[Filter], SamplingFilterRadii[Filter],
    WrapEdges);
end;

var
  FullEdge: Boolean = True;

{ The following resampling code is modified and extended code from Graphics32
  library by Alex A. Denisov.}
function BuildMappingTable(DstLow, DstHigh, SrcLow, SrcHigh, SrcImageWidth: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean): TMappingTable;
var
  I, J, K, N: LongInt;
  Left, Right, SrcWidth, DstWidth: LongInt;
  Weight, Scale, Center: Single;
begin
  Result := nil;
  SrcWidth := SrcHigh - SrcLow;
  DstWidth := DstHigh - DstLow;

  // Check some special cases
  if SrcWidth = 1 then
  begin
    SetLength(Result, DstWidth);
    for I := 0 to DstWidth - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 1.0;
    end;
    Exit;
  end
  else
  if (SrcWidth = 0) or (DstWidth = 0) then
    Exit;

  if FullEdge then
    Scale := DstWidth / SrcWidth
  else
    Scale := (DstWidth - 1) / (SrcWidth - 1);

  SetLength(Result, DstWidth);

  // Pre-calculate filter contributions for a row or column
  if Scale = 0.0 then
  begin
    Assert(Length(Result) = 1);
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLow + SrcHigh) div 2;
    Result[0][0].Weight := 1.0;
  end
  else if Scale < 1.0 then
  begin
    // Sub-sampling - scales from bigger to smaller 
    Radius := Radius / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) / Scale
      else
        Center := SrcLow + I / Scale;
      Left := Floor(Center - Radius);
      Right := Ceil(Center + Radius);
      for J := Left to Right do
      begin
        Weight := Filter((Center - J) * Scale) * Scale;
        if Weight <> 0.0 then
        begin
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := ClampInt(J, SrcLow, SrcHigh - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Floor(Center);
        Result[I][0].Weight := 1.0;
      end;
    end;
  end
  else // if Scale > 1.0 then
  begin
    // Super-sampling - scales from smaller to bigger
    Scale := 1.0 / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) * Scale
      else
        Center := SrcLow + I * Scale;
      Left := Floor(Center - Radius);
      Right := Ceil(Center + Radius);
      for J := Left to Right do
      begin
        Weight := Filter(Center - J);
        if Weight <> 0.0 then
        begin
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);

          if WrapEdges then
          begin
            if J < 0 then
              N := SrcImageWidth + J
            else if J >= SrcImageWidth then
              N := J - SrcImageWidth
            else
              N := ClampInt(J, SrcLow, SrcHigh - 1);
          end
          else
            N := ClampInt(J, SrcLow, SrcHigh - 1);

          Result[I][K].Pos := N;
          Result[I][K].Weight := Weight;
        end;
      end;
    end;
  end;
end;

procedure FindExtremes(const Map: TMappingTable; var MinPos, MaxPos: LongInt);
var
  I, J: LongInt;
begin
  if Length(Map) > 0 then
  begin
    MinPos := Map[0][0].Pos;
    MaxPos := MinPos;
    for I := 0 to Length(Map) - 1 do
      for J := 0 to Length(Map[I]) - 1 do
      begin
        if MinPos > Map[I][J].Pos then
          MinPos := Map[I][J].Pos;
        if MaxPos < Map[I][J].Pos then
          MaxPos := Map[I][J].Pos;
      end;
  end;
end;

procedure StretchResample(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean);
var
  MapX, MapY: TMappingTable;
  I, J, X, Y: LongInt;
  XMinimum, XMaximum: LongInt;
  LineBufferFP: array of TColorFPRec;
  ClusterX, ClusterY: TCluster;
  Weight, AccumA, AccumR, AccumG, AccumB: Single;
  DstLine: PByte;
  SrcFloat: TColorFPRec;
  Info: TImageFormatInfo;
  BytesPerChannel: Integer;
begin
  GetImageFormatInfo(SrcImage.Format, Info);
  Assert(SrcImage.Format = DstImage.Format);
  Assert(not Info.IsSpecial and not Info.IsIndexed);
  BytesPerChannel := Info.BytesPerPixel div Info.ChannelCount;

  // Create horizontal and vertical mapping tables
  MapX := BuildMappingTable(DstX, DstX + DstWidth, SrcX, SrcX + SrcWidth,
    SrcImage.Width, Filter, Radius, WrapEdges);
  MapY := BuildMappingTable(DstY, DstY + DstHeight, SrcY, SrcY + SrcHeight,
    SrcImage.Height, Filter, Radius, WrapEdges);

  if (MapX = nil) or (MapY = nil) then
    Exit;

  try
    // Find min and max X coords of pixels that will contribute to target image
    FindExtremes(MapX, XMinimum, XMaximum);
    SetLength(LineBufferFP, XMaximum - XMinimum + 1);

    for J := 0 to DstHeight - 1 do
    begin
      // First for each pixel in the current line sample vertically
      // and store results in LineBuffer. Then sample horizontally
      // using values in LineBuffer.
      ClusterY := MapY[J];
      for X := XMinimum to XMaximum do
      begin
        // Clear accumulators
        AccumA := 0;
        AccumR := 0;
        AccumG := 0;
        AccumB := 0;
        // For each pixel in line compute weighted sum of pixels
        // in source column that will contribute to this pixel
        for Y := 0 to Length(ClusterY) - 1 do
        begin
          // Accumulate this pixel's weighted value
          Weight := ClusterY[Y].Weight;
          SrcFloat := Info.GetPixelFP(@PByteArray(SrcImage.Bits)[(ClusterY[Y].Pos * SrcImage.Width + X) * Info.BytesPerPixel], @Info, nil);
          AccumA := AccumA + SrcFloat.A * Weight;
          AccumR := AccumR + SrcFloat.R * Weight;
          AccumG := AccumG + SrcFloat.G * Weight;
          AccumB := AccumB + SrcFloat.B * Weight;
        end;
        // Store accumulated value for this pixel in buffer
        with LineBufferFP[X - XMinimum] do
        begin
          A := AccumA;
          R := AccumR;
          G := AccumG;
          B := AccumB;
        end;
      end;

      DstLine := @PByteArray(DstImage.Bits)[((J + DstY) * DstImage.Width + DstX) * Info.BytesPerPixel];
      // Now compute final colors for target pixels in the current row
      // by sampling horizontally
      for I := 0 to DstWidth - 1 do
      begin
        ClusterX := MapX[I];
        // Clear accumulator
        AccumA := 0;
        AccumR := 0;
        AccumG := 0;
        AccumB := 0;
        // Compute weighted sum of values (which are already
        // computed weighted sums of pixels in source columns stored in LineBuffer)
        // that will contribute to the current target pixel
        for X := 0 to Length(ClusterX) - 1 do
        begin
          Weight := ClusterX[X].Weight;
          with LineBufferFP[ClusterX[X].Pos - XMinimum] do
          begin
            AccumA := AccumA + A * Weight;
            AccumR := AccumR + R * Weight;
            AccumG := AccumG + G * Weight;
            AccumB := AccumB + B * Weight;
          end;
        end;

        // Now compute final color to be written to dest image
        SrcFloat.A := AccumA;
        SrcFloat.R := AccumR;
        SrcFloat.G := AccumG;
        SrcFloat.B := AccumB;

        Info.SetPixelFP(DstLine, @Info, nil, SrcFloat);
        Inc(DstLine, Info.BytesPerPixel);
      end;
    end;

  finally
    MapX := nil;
    MapY := nil;
  end;
end;

procedure FillMipMapLevel(const BiggerLevel: TImageData; Width, Height: LongInt;
  var SmallerLevel: TImageData);
var
  Filter: TSamplingFilter;
  Info: TImageFormatInfo;
  CompatibleCopy: TImageData;
begin
  Assert(TestImage(BiggerLevel));
  Filter := TSamplingFilter(GetOption(ImagingMipMapFilter));

  // If we have special format image we must create copy to allow pixel access
  GetImageFormatInfo(BiggerLevel.Format, Info);
  if Info.IsSpecial then
  begin
    InitImage(CompatibleCopy);
    CloneImage(BiggerLevel, CompatibleCopy);
    ConvertImage(CompatibleCopy, ifDefault);
  end
  else
    CompatibleCopy := BiggerLevel;

  // Create new smaller image
  NewImage(Width, Height, CompatibleCopy.Format, SmallerLevel);
  GetImageFormatInfo(CompatibleCopy.Format, Info);
  // If input is indexed we must copy its palette
  if Info.IsIndexed then
    CopyPalette(CompatibleCopy.Palette, SmallerLevel.Palette, 0, 0, Info.PaletteEntries);

  if (Filter = sfNearest) or Info.IsIndexed then
  begin
    StretchNearest(CompatibleCopy, 0, 0, CompatibleCopy.Width, CompatibleCopy.Height,
      SmallerLevel, 0, 0, Width, Height);
  end
  else
  begin
    StretchResample(CompatibleCopy, 0, 0, CompatibleCopy.Width, CompatibleCopy.Height,
      SmallerLevel, 0, 0, Width, Height, Filter);
  end;

  // Free copy and convert result to special format if necessary
  if CompatibleCopy.Format <> BiggerLevel.Format then
  begin
    ConvertImage(SmallerLevel, BiggerLevel.Format);
    FreeImage(CompatibleCopy);
  end;
end;


{ Various format support functions }

procedure CopyPixel(Src, Dest: Pointer; BytesPerPixel: LongInt);
begin
  case BytesPerPixel of
    1: PByte(Dest)^ := PByte(Src)^;
    2: PWord(Dest)^ := PWord(Src)^;
    3: PColor24Rec(Dest)^ := PColor24Rec(Src)^;
    4: PUInt32(Dest)^ := PUInt32(Src)^;
    6: PColor48Rec(Dest)^ := PColor48Rec(Src)^;
    8: PInt64(Dest)^ := PInt64(Src)^;
    12: PColor96FPRec(Dest)^ := PColor96FPRec(Src)^;
    16: PColorFPRec(Dest)^ := PColorFPRec(Src)^;
  end;
end;

function ComparePixels(PixelA, PixelB: Pointer; BytesPerPixel: LongInt): Boolean;
begin
  case BytesPerPixel of
    1: Result := PByte(PixelA)^ = PByte(PixelB)^;
    2: Result := PWord(PixelA)^ = PWord(PixelB)^;
    3: Result := (PWord(PixelA)^ = PWord(PixelB)^) and (PColor24Rec(PixelA).R = PColor24Rec(PixelB).R);
    4: Result := PUInt32(PixelA)^ = PUInt32(PixelB)^;
    6: Result := (PUInt32(PixelA)^ = PUInt32(PixelB)^) and (PColor48Rec(PixelA).R = PColor48Rec(PixelB).R);
    8: Result := PInt64(PixelA)^ = PInt64(PixelB)^;
    12: Result := (PFloatHelper(PixelA).Data = PFloatHelper(PixelB).Data) and
          (PFloatHelper(PixelA).Data32 = PFloatHelper(PixelB).Data32);
    16: Result := (PFloatHelper(PixelA).Data = PFloatHelper(PixelB).Data) and
          (PFloatHelper(PixelA).Data64 = PFloatHelper(PixelB).Data64);
  else
    Result := False;
  end;
end;

procedure TranslatePixel(SrcPixel, DstPixel: Pointer; SrcFormat,
  DstFormat: TImageFormat; SrcPalette, DstPalette: PPalette32);
var
  SrcInfo, DstInfo: PImageFormatInfo;
  PixFP: TColorFPRec;
begin
  SrcInfo := FInfos[SrcFormat];
  DstInfo := FInfos[DstFormat];

  PixFP := GetPixelFPGeneric(SrcPixel, SrcInfo, SrcPalette);
  SetPixelFPGeneric(DstPixel, DstInfo, DstPalette, PixFP);
end;

procedure ClampFloatPixel(var PixF: TColorFPRec);
begin
  if PixF.A > 1.0 then
    PixF.A := 1.0;
  if PixF.R > 1.0 then
    PixF.R := 1.0;
  if PixF.G > 1.0 then
    PixF.G := 1.0;
  if PixF.B > 1.0 then
    PixF.B := 1.0;

  if PixF.A < 0.0 then
    PixF.A := 0.0;
  if PixF.R < 0.0 then
    PixF.R := 0.0;
  if PixF.G < 0.0 then
    PixF.G := 0.0;
  if PixF.B < 0.0 then
    PixF.B := 0.0;
end;

procedure ConvertToPixel32(SrcPix: PByte; DestPix: PColor32Rec;
  const SrcInfo: TImageFormatInfo; SrcPalette: PPalette32);
begin
  case SrcInfo.Format of
    ifIndex8:
      begin
        DestPix^ := SrcPalette[SrcPix^];
      end;
    ifGray8:
      begin
        DestPix.R := SrcPix^;
        DestPix.G := SrcPix^;
        DestPix.B := SrcPix^;
        DestPix.A := 255;
      end;
    ifA8Gray8:
      begin
        DestPix.R := SrcPix^;
        DestPix.G := SrcPix^;
        DestPix.B := SrcPix^;
        DestPix.A := PWordRec(SrcPix).High;
      end;
    ifGray16:
      begin
        DestPix.R := PWord(SrcPix)^ shr 8;
        DestPix.G := DestPix.R;
        DestPix.B := DestPix.R;
        DestPix.A := 255;
      end;
    ifR8G8B8:
      begin
        DestPix.Color24Rec := PColor24Rec(SrcPix)^;
        DestPix.A := 255;
      end;
    ifA8R8G8B8:
      begin
        DestPix^ := PColor32Rec(SrcPix)^;
      end;
    ifR16G16B16:
      begin
        DestPix.R := PColor48Rec(SrcPix).R shr 8;
        DestPix.G := PColor48Rec(SrcPix).G shr 8;
        DestPix.B := PColor48Rec(SrcPix).B shr 8;
        DestPix.A := 255;
      end;
    ifA16R16G16B16:
      begin
        DestPix.R := PColor64Rec(SrcPix).R shr 8;
        DestPix.G := PColor64Rec(SrcPix).G shr 8;
        DestPix.B := PColor64Rec(SrcPix).B shr 8;
        DestPix.A := PColor64Rec(SrcPix).A shr 8;
      end;
  else
    DestPix^ := SrcInfo.GetPixel32(SrcPix, @SrcInfo, SrcPalette);
  end;
end;

procedure AddPadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);
var
  I, W: LongInt;
begin
  W := Width * Bpp;
  for I := 0 to Height - 1 do
    Move(PByteArray(DataIn)[I * W], PByteArray(DataOut)[I * WidthBytes], W);
end;

procedure RemovePadBytes(DataIn: Pointer; DataOut: Pointer; Width, Height,
  Bpp, WidthBytes: LongInt);
var
  I, W: LongInt;
begin
  W := Width * Bpp;
  for I := 0 to Height - 1 do
    Move(PByteArray(DataIn)[I * WidthBytes], PByteArray(DataOut)[I * W], W);
end;

procedure Convert1To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
const
  Mask1: array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  Shift1: array[0..7] of Byte = (7, 6, 5, 4, 3, 2, 1, 0);
  Scaling: Byte = 255;
var
  X, Y: LongInt;
  InArray: PByteArray absolute DataIn;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      DataOut^ := (InArray[Y * WidthBytes + X shr 3] and Mask1[X and 7]) shr Shift1[X and 7];
      if ScaleTo8Bits then
        DataOut^ := DataOut^ * Scaling;
      Inc(DataOut);
    end;
end;

procedure Convert2To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
const
  Mask2: array[0..3] of Byte = ($C0, $30, $0C, $03);
  Shift2: array[0..3] of Byte = (6, 4, 2, 0);
  Scaling: Byte = 85;
var
  X, Y: LongInt;
  InArray: PByteArray absolute DataIn;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      DataOut^ := (InArray[Y * WidthBytes + X shr 2] and Mask2[X and 3]) shr Shift2[X and 3];
      if ScaleTo8Bits then
        DataOut^ := DataOut^ * Scaling;
      Inc(DataOut);
    end;
end;

procedure Convert4To8(DataIn, DataOut: PByte; Width, Height,
  WidthBytes: LongInt; ScaleTo8Bits: Boolean);
const
  Mask4: array[0..1] of Byte = ($F0, $0F);
  Shift4: array[0..1] of Byte = (4, 0);
  Scaling: Byte = 17;
var
  X, Y: LongInt;
  InArray: PByteArray absolute DataIn;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      DataOut^ := (InArray[Y * WidthBytes + X shr 1] and  Mask4[X and 1]) shr Shift4[X and 1];
      if ScaleTo8Bits then
        DataOut^ := DataOut^ * Scaling;
      Inc(DataOut);
    end;
end;

function Has16BitImageAlpha(NumPixels: LongInt; Data: PWord): Boolean;
var
  I: LongInt;
begin
  Result := False;
  for I := 0 to NumPixels - 1 do
  begin
    if Data^ >= 1 shl 15 then
    begin
      Result := True;
      Exit;
    end;
    Inc(Data);
  end;
end;

function Has32BitImageAlpha(NumPixels: LongInt; Data: PUInt32): Boolean;
var
  I: LongInt;
begin
  Result := False;
  for I := 0 to NumPixels - 1 do
  begin
    if Data^ >= 1 shl 24 then
    begin
      Result := True;
      Exit;
    end;
    Inc(Data);
  end;
end;

function PaletteHasAlpha(Palette: PPalette32; PaletteEntries: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to PaletteEntries - 1 do
  begin
    if Palette[I].A <> 255 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function PaletteIsGrayScale(Palette: PPalette32; PaletteEntries: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to PaletteEntries - 1 do
  begin
    if (Palette[I].R <> Palette[I].G) or (Palette[I].R <> Palette[I].B)  then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function GetScanLine(ImageBits: Pointer; const FormatInfo: TImageFormatInfo;
  LineWidth, Index: LongInt): Pointer;
var
  LineBytes: LongInt;
begin
  Assert(not FormatInfo.IsSpecial);
  LineBytes := FormatInfo.GetPixelsSize(FormatInfo.Format, LineWidth, 1);
  Result := @PByteArray(ImageBits)[Index * LineBytes];
end;

function IsImageFormatValid(Format: TImageFormat): Boolean;
begin
  Result := FInfos[Format] <> nil;
end;

const
  HalfMin:     Single = 5.96046448e-08; // Smallest positive half
  HalfMinNorm: Single = 6.10351562e-05; // Smallest positive normalized half
  HalfMax:     Single = 65504.0;        // Largest positive half
  HalfEpsilon: Single = 0.00097656;     // Smallest positive e for which half (1.0 + e) != half (1.0)
  HalfNaN:     THalfFloat = 65535;
  HalfPosInf:  THalfFloat = 31744;
  HalfNegInf:  THalfFloat = 64512;


{
  Half/Float conversions inspired by half class from OpenEXR library.

  Float (Pascal Single type) is an IEEE 754 single-precision
  floating point number.

  Bit layout of Single:

    31 (msb)
    |
    | 30     23
    | |      |
    | |      | 22                    0 (lsb)
    | |      | |                     |
    X XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXX
    s e        m

  Bit layout of half:

    15 (msb)
    |
    | 14  10
    | |   |
    | |   | 9        0 (lsb)
    | |   | |        |
    X XXXXX XXXXXXXXXX
    s e     m

  S is the sign-bit, e is the exponent and m is the significand (mantissa).
}

function HalfToFloat(Half: THalfFloat): Single;
var
  Dst, Sign, Mantissa: UInt32;
  Exp: Int32;
begin
  // Extract sign, exponent, and mantissa from half number
  Sign := Half shr 15;
  Exp := (Half and $7C00) shr 10;
  Mantissa := Half and 1023;

  if (Exp > 0) and (Exp < 31) then
  begin
    // Common normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (UInt32(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // Zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // Denormalized number - renormalize it
    while (Mantissa and $00000400) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00000400;
    // Now assemble normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (UInt32(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 31) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else //if (Exp = 31) and (Mantisa <> 0) then
  begin
    // Not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
  end;

  // Reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;

function FloatToHalf(Float: Single): THalfFloat;
var
  Src: UInt32;
  Sign, Exp, Mantissa: Int32;
begin
  Src := PUInt32(@Float)^;
  // Extract sign, exponent, and mantissa from Single number
  Sign := Src shr 31;
  Exp := Int32((Src and $7F800000) shr 23) - 127 + 15;
  Mantissa := Src and $007FFFFF;

  if (Exp > 0) and (Exp < 30) then
  begin
    // Simple case - round the significand and combine it with the sign and exponent
    Result := (Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13);
  end
  else if Src = 0 then
  begin
    // Input float is zero - return zero
    Result := 0;
  end
  else
  begin
    // Difficult case - lengthy conversion
    if Exp <= 0 then
    begin
      if Exp < -10 then
      begin
        // Input float's value is less than HalfMin, return zero
        Result := 0;
      end
      else
      begin
        // Float is a normalized Single whose magnitude is less than HalfNormMin.
        // We convert it to denormalized half.
        Mantissa := (Mantissa or $00800000) shr (1 - Exp);
        // Round to nearest
        if (Mantissa and $00001000) > 0 then
          Mantissa := Mantissa + $00002000;
        // Assemble Sign and Mantissa (Exp is zero to get denormalized number)
        Result := (Sign shl 15) or (Mantissa shr 13);
      end;
    end
    else if Exp = 255 - 127 + 15 then
    begin
      if Mantissa = 0 then
      begin
        // Input float is infinity, create infinity half with original sign
        Result := (Sign shl 15) or $7C00;
      end
      else
      begin
        // Input float is NaN, create half NaN with original sign and mantissa
        Result := (Sign shl 15) or $7C00 or (Mantissa shr 13);
      end;
    end
    else
    begin
      // Exp is > 0 so input float is normalized Single

      // Round to nearest
      if (Mantissa and $00001000) > 0 then
      begin
        Mantissa := Mantissa + $00002000;
        if (Mantissa and $00800000) > 0 then
        begin
          Mantissa := 0;
          Exp := Exp + 1;
        end;
      end;

      if Exp > 30 then
      begin
        // Exponent overflow - return infinity half
        Result := (Sign shl 15) or $7C00;
      end
      else
        // Assemble normalized half
        Result := (Sign shl 15) or (Exp shl 10) or (Mantissa shr 13);
    end;
  end;
end;

function ColorHalfToFloat(ColorHF: TColorHFRec): TColorFPRec;
begin
  Result.A := HalfToFloat(ColorHF.A);
  Result.R := HalfToFloat(ColorHF.R);
  Result.G := HalfToFloat(ColorHF.G);
  Result.B := HalfToFloat(ColorHF.B);
end;

function ColorFloatToHalf(ColorFP: TColorFPRec): TColorHFRec;
begin
  Result.A := FloatToHalf(ColorFP.A);
  Result.R := FloatToHalf(ColorFP.R);
  Result.G := FloatToHalf(ColorFP.G);
  Result.B := FloatToHalf(ColorFP.B);
end;

function Color32ToGray(Color32: TColor32): Byte;
begin
  Result := Round(GrayConv.R * TColor32Rec(Color32).R +
                  GrayConv.G * TColor32Rec(Color32).G +
                  GrayConv.B * TColor32Rec(Color32).B);
end;

procedure VisualizePalette(Pal: PPalette32; Entries: Integer; out PalImage: TImageData);
var
  I: Integer;
  Pix: PColor32;
begin
  InitImage(PalImage);
  NewImage(Entries, 1, ifA8R8G8B8, PalImage);
  Pix := PalImage.Bits;
  for I := 0 to Entries - 1 do
  begin
    Pix^ := Pal[I].Color;
    Inc(Pix);
  end;       
end;


{ Pixel readers/writers for different image formats }

procedure ChannelGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColor64Rec);
var
  A, R, G, B: Byte;
begin
  FillChar(Pix, SizeOf(Pix), 0);
  // returns 64 bit color value with 16 bits for each channel
  case SrcInfo.BytesPerPixel of
    1:
      begin
        PFGetARGB(SrcInfo.PixelFormat^, Src^, A, R, G, B);
        Pix.A := A shl 8;
        Pix.R := R shl 8;
        Pix.G := G shl 8;
        Pix.B := B shl 8;
      end;
    2:
      begin
        PFGetARGB(SrcInfo.PixelFormat^, PWord(Src)^, A, R, G, B);
        Pix.A := A shl 8;
        Pix.R := R shl 8;
        Pix.G := G shl 8;
        Pix.B := B shl 8;
      end;
    3:
      with Pix do
      begin
        R := MulDiv(PColor24Rec(Src).R, 65535, 255);
        G := MulDiv(PColor24Rec(Src).G, 65535, 255);
        B := MulDiv(PColor24Rec(Src).B, 65535, 255);
      end;
    4:
      with Pix do
      begin
        A := MulDiv(PColor32Rec(Src).A, 65535, 255);
        R := MulDiv(PColor32Rec(Src).R, 65535, 255);
        G := MulDiv(PColor32Rec(Src).G, 65535, 255);
        B := MulDiv(PColor32Rec(Src).B, 65535, 255);
      end;
    6:
      with Pix do
      begin
        R := PColor48Rec(Src).R;
        G := PColor48Rec(Src).G;
        B := PColor48Rec(Src).B;
      end;
    8: Pix.Color := PColor64(Src)^;
  end;
  // if src has no alpha, we set it to max (otherwise we would have to
  // test if dest has alpha or not in each ChannelToXXX function)
  if not SrcInfo.HasAlphaChannel then
    Pix.A := 65535;

  if SrcInfo.IsRBSwapped then
    SwapValues(Pix.R, Pix.B);
end;

procedure ChannelSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColor64Rec);
var
  PixW: TColor64Rec;
begin
  PixW := Pix;
  if DstInfo.IsRBSwapped then
    SwapValues(PixW.R, PixW.B);
  // Pix contains 64 bit color value with 16 bit for each channel
  case DstInfo.BytesPerPixel of
    1: Dst^ := PFSetARGB(DstInfo.PixelFormat^, PixW.A shr 8,
        PixW.R shr 8, PixW.G shr 8, PixW.B shr 8);
    2: PWord(Dst)^ := PFSetARGB(DstInfo.PixelFormat^, PixW.A shr 8,
        PixW.R shr 8, PixW.G shr 8, PixW.B shr 8);
    3:
      with PColor24Rec(Dst)^ do
      begin
        R := MulDiv(PixW.R, 255, 65535);
        G := MulDiv(PixW.G, 255, 65535);
        B := MulDiv(PixW.B, 255, 65535);
      end;
    4:
      with PColor32Rec(Dst)^ do
      begin
        A := MulDiv(PixW.A, 255, 65535);
        R := MulDiv(PixW.R, 255, 65535);
        G := MulDiv(PixW.G, 255, 65535);
        B := MulDiv(PixW.B, 255, 65535);
      end;
    6:
      with PColor48Rec(Dst)^ do
      begin
        R := PixW.R;
        G := PixW.G;
        B := PixW.B;
      end;
    8: PColor64(Dst)^ := PixW.Color;
  end;
end;

procedure GrayGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Gray: TColor64Rec; var Alpha: Word);
begin
  FillChar(Gray, SizeOf(Gray), 0);
  // Source alpha is scaled to 16 bits and stored in Alpha,
  // grayscale value is scaled to 64 bits and stored in Gray
  case SrcInfo.BytesPerPixel of
    1: Gray.A := MulDiv(Src^, 65535, 255);
    2:
      if SrcInfo.HasAlphaChannel then
        with PWordRec(Src)^ do
        begin
          Alpha := MulDiv(High, 65535, 255);
          Gray.A := MulDiv(Low, 65535, 255);
        end
      else
        Gray.A := PWord(Src)^;
    4:
      if SrcInfo.HasAlphaChannel then
        with PUInt32Rec(Src)^ do
        begin
          Alpha := High;
          Gray.A := Low;
        end
      else
        with PUInt32Rec(Src)^ do
        begin
          Gray.A := High;
          Gray.R := Low;
        end;
    8: Gray.Color := PColor64(Src)^;
  end;
  // if src has no alpha, we set it to max (otherwise we would have to
  // test if dest has alpha or not in each GrayToXXX function)
  if not SrcInfo.HasAlphaChannel then
    Alpha := 65535;
end;

procedure GraySetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Gray: TColor64Rec; Alpha: Word);
begin
  // Gray contains grayscale value scaled to 64 bits, Alpha contains
  // alpha value scaled to 16 bits
  case DstInfo.BytesPerPixel of
    1: Dst^ := MulDiv(Gray.A, 255, 65535);
    2:
      if DstInfo.HasAlphaChannel then
        with PWordRec(Dst)^ do
        begin
          High := MulDiv(Alpha, 255, 65535);
          Low := MulDiv(Gray.A, 255, 65535);
        end
      else
        PWord(Dst)^ := Gray.A;
    4:
      if DstInfo.HasAlphaChannel then
        with PUInt32Rec(Dst)^ do
        begin
          High := Alpha;
          Low := Gray.A;
        end
      else
        with PUInt32Rec(Dst)^ do
        begin
          High := Gray.A;
          Low := Gray.R;
        end;
    8: PColor64(Dst)^ := Gray.Color;
  end;
end;

procedure FloatGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Pix: TColorFPRec);
var
  PixHF: TColorHFRec;
begin
  Assert(SrcInfo.BytesPerPixel in [2, 4, 8, 12, 16]);

  if SrcInfo.BytesPerPixel in [4, 12, 16] then
  begin
    // IEEE 754 single-precision channels
    FillChar(Pix, SizeOf(Pix), 0);
    case SrcInfo.BytesPerPixel of
      4: Pix.R := PSingle(Src)^;
      12: Pix.Color96Rec := PColor96FPRec(Src)^;
      16: Pix := PColorFPRec(Src)^;
    end;
  end
  else
  begin
    // Half float channels
    FillChar(PixHF, SizeOf(PixHF), 0);
    case SrcInfo.BytesPerPixel of
      2: PixHF.R := PHalfFloat(Src)^;
      8: PixHF := PColorHFRec(Src)^;
    end;
    Pix := ColorHalfToFloat(PixHF);
  end;

  // If src has no alpha, we set it to max (otherwise we would have to
  // test if dest has alpha or not in each FloatToXXX function)
  if not SrcInfo.HasAlphaChannel then
    Pix.A := 1.0;
  if SrcInfo.IsRBSwapped then
    SwapValues(Pix.R, Pix.B);
end;

procedure FloatSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  const Pix: TColorFPRec);
var
  PixW: TColorFPRec;
  PixHF: TColorHFRec;
begin
  Assert(DstInfo.BytesPerPixel in [2, 4, 8, 12, 16]);

  PixW := Pix;
  if DstInfo.IsRBSwapped then
    SwapValues(PixW.R, PixW.B);

  if DstInfo.BytesPerPixel in [4, 12, 16] then
  begin
    case DstInfo.BytesPerPixel of
      4:  PSingle(Dst)^ := PixW.R;
      12: PColor96FPRec(Dst)^:= PixW.Color96Rec;
      16: PColorFPRec(Dst)^ := PixW;
    end;
  end
  else
  begin
    PixHF := ColorFloatToHalf(PixW);
    case DstInfo.BytesPerPixel of
      2: PHalfFloat(Dst)^ := PixHF.R;
      8: PColorHFRec(Dst)^ := PixHF;
    end;
  end;
end;

procedure IndexGetSrcPixel(Src: PByte; SrcInfo: PImageFormatInfo;
  var Index: UInt32);
begin
  case SrcInfo.BytesPerPixel of
    1: Index := Src^;
  end;
end;

procedure IndexSetDstPixel(Dst: PByte; DstInfo: PImageFormatInfo;
  Index: UInt32);
begin
  case DstInfo.BytesPerPixel of
    1: Dst^ := Byte(Index);
    2: PWord(Dst)^ := Word(Index);
    4: PUInt32(Dst)^ := Index;
  end;
end;


{ Pixel readers/writers for 32bit and FP colors}

function GetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColor32Rec;
var
  Pix64: TColor64Rec;
  PixF: TColorFPRec;
  Alpha: Word;
  Index: UInt32;
begin
  if Info.Format = ifA8R8G8B8 then
  begin
    Result := PColor32Rec(Bits)^
  end
  else if Info.Format = ifR8G8B8 then
  begin
    PColor24Rec(@Result)^ := PColor24Rec(Bits)^;
    Result.A := $FF;
  end
  else if Info.IsFloatingPoint then
  begin
    FloatGetSrcPixel(Bits, Info, PixF);
    Result.A := ClampToByte(Round(PixF.A * 255.0));
    Result.R := ClampToByte(Round(PixF.R * 255.0));
    Result.G := ClampToByte(Round(PixF.G * 255.0));
    Result.B := ClampToByte(Round(PixF.B * 255.0));
  end
  else if Info.HasGrayChannel then
  begin
    GrayGetSrcPixel(Bits, Info, Pix64, Alpha);
    Result.A := MulDiv(Alpha, 255, 65535);
    Result.R := MulDiv(Pix64.A, 255, 65535);
    Result.G := MulDiv(Pix64.A, 255, 65535);
    Result.B := MulDiv(Pix64.A, 255, 65535);
  end
  else if Info.IsIndexed then
  begin
    IndexGetSrcPixel(Bits, Info, Index);
    Result := Palette[Index];
  end
  else
  begin
    ChannelGetSrcPixel(Bits, Info, Pix64);
    Result.A := MulDiv(Pix64.A, 255, 65535);
    Result.R := MulDiv(Pix64.R, 255, 65535);
    Result.G := MulDiv(Pix64.G, 255, 65535);
    Result.B := MulDiv(Pix64.B, 255, 65535);
  end;
end;

procedure SetPixel32Generic(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColor32Rec);
var
  Pix64: TColor64Rec;
  PixF: TColorFPRec;
  Alpha: Word;
  Index: UInt32;
begin
  if Info.Format = ifA8R8G8B8 then
  begin
    PColor32Rec(Bits)^ := Color
  end
  else if Info.Format = ifR8G8B8 then
  begin
    PColor24Rec(Bits)^ := Color.Color24Rec;
  end
  else if Info.IsFloatingPoint then
  begin
    PixF.A := Color.A * OneDiv8Bit;
    PixF.R := Color.R * OneDiv8Bit;
    PixF.G := Color.G * OneDiv8Bit;
    PixF.B := Color.B * OneDiv8Bit;
    FloatSetDstPixel(Bits, Info, PixF);
  end
  else if Info.HasGrayChannel then
  begin
    Alpha := MulDiv(Color.A, 65535, 255);
    Pix64.Color := 0;
    Pix64.A := MulDiv(Round(GrayConv.R * Color.R + GrayConv.G * Color.G +
      GrayConv.B * Color.B), 65535, 255);
    GraySetDstPixel(Bits, Info, Pix64, Alpha);
  end
  else if Info.IsIndexed then
  begin
    Index := FindColor(Palette, Info.PaletteEntries, Color.Color);
    IndexSetDstPixel(Bits, Info, Index);
  end
  else
  begin
    Pix64.A := MulDiv(Color.A, 65535, 255);
    Pix64.R := MulDiv(Color.R, 65535, 255);
    Pix64.G := MulDiv(Color.G, 65535, 255);
    Pix64.B := MulDiv(Color.B, 65535, 255);
    ChannelSetDstPixel(Bits, Info, Pix64);
  end;
end;

function GetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec;
var
  Pix32: TColor32Rec;
  Pix64: TColor64Rec;
  Alpha: Word;
  Index: UInt32;
begin
  if Info.IsFloatingPoint then
  begin
    FloatGetSrcPixel(Bits, Info, Result);
  end
  else if Info.HasGrayChannel then
  begin
    GrayGetSrcPixel(Bits, Info, Pix64, Alpha);
    Result.A := Alpha * OneDiv16Bit;
    Result.R := Pix64.A * OneDiv16Bit;
    Result.G := Pix64.A * OneDiv16Bit;
    Result.B := Pix64.A * OneDiv16Bit;
  end
  else if Info.IsIndexed then
  begin
    IndexGetSrcPixel(Bits, Info, Index);
    Pix32 := Palette[Index];
    Result.A := Pix32.A * OneDiv8Bit;
    Result.R := Pix32.R * OneDiv8Bit;
    Result.G := Pix32.G * OneDiv8Bit;
    Result.B := Pix32.B * OneDiv8Bit;
  end
  else
  begin
    ChannelGetSrcPixel(Bits, Info, Pix64);
    Result.A := Pix64.A * OneDiv16Bit;
    Result.R := Pix64.R * OneDiv16Bit;
    Result.G := Pix64.G * OneDiv16Bit;
    Result.B := Pix64.B * OneDiv16Bit;
  end;
end;

procedure SetPixelFPGeneric(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec);
var
  Pix32: TColor32Rec;
  Pix64: TColor64Rec;
  Alpha: Word;
  Index: UInt32;
begin
  if Info.IsFloatingPoint then
  begin
    FloatSetDstPixel(Bits, Info, Color);
  end
  else if Info.HasGrayChannel then
  begin
    Alpha := ClampToWord(Round(Color.A * 65535.0));
    Pix64.Color := 0;
    Pix64.A := ClampToWord(Round((GrayConv.R * Color.R + GrayConv.G * Color.G +
      GrayConv.B * Color.B) * 65535.0));
    GraySetDstPixel(Bits, Info, Pix64, Alpha);
  end
  else if Info.IsIndexed then
  begin
    Pix32.A := ClampToByte(Round(Color.A * 255.0));
    Pix32.R := ClampToByte(Round(Color.R * 255.0));
    Pix32.G := ClampToByte(Round(Color.G * 255.0));
    Pix32.B := ClampToByte(Round(Color.B * 255.0));
    Index := FindColor(Palette, Info.PaletteEntries, Pix32.Color);
    IndexSetDstPixel(Bits, Info, Index);
  end
  else
  begin
    Pix64.A := ClampToWord(Round(Color.A * 65535.0));
    Pix64.R := ClampToWord(Round(Color.R * 65535.0));
    Pix64.G := ClampToWord(Round(Color.G * 65535.0));
    Pix64.B := ClampToWord(Round(Color.B * 65535.0));
    ChannelSetDstPixel(Bits, Info, Pix64);
  end;
end;


{ Image format conversion functions }

procedure ChannelToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Pix64: TColor64Rec;
begin
  // two most common conversions (RGB->ARGB and ARGB->RGB for 24/32 bit
  // images) are made separately from general ARGB conversion to
  // make them faster
  if (SrcInfo.BytesPerPixel = 3) and (DstInfo.BytesPerPixel = 4) then
  for I := 0 to NumPixels - 1 do
    begin
      PColor24Rec(Dst)^ := PColor24Rec(Src)^;
      if DstInfo.HasAlphaChannel then
        PColor32Rec(Dst).A := 255;
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
  if (SrcInfo.BytesPerPixel = 4) and (DstInfo.BytesPerPixel = 3) then
    for I := 0 to NumPixels - 1 do
    begin
      PColor24Rec(Dst)^ := PColor24Rec(Src)^;
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
    for I := 0 to NumPixels - 1 do
    begin
      // general ARGB conversion
      ChannelGetSrcPixel(Src, SrcInfo, Pix64);
      ChannelSetDstPixel(Dst, DstInfo, Pix64);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure ChannelToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Pix64: TColor64Rec;
  Alpha: Word;
begin
  // two most common conversions (R8G8B8->Gray8 nad A8R8G8B8->Gray8)
  // are made separately from general conversions to make them faster
  if (SrcInfo.BytesPerPixel in [3, 4]) and (DstInfo.Format = ifGray8) then
    for I := 0 to NumPixels - 1 do
    begin
      Dst^ := Round(GrayConv.R * PColor24Rec(Src).R + GrayConv.G * PColor24Rec(Src).G +
        GrayConv.B * PColor24Rec(Src).B);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
    for I := 0 to NumPixels - 1 do
    begin
      ChannelGetSrcPixel(Src, SrcInfo, Pix64);

      // alpha is saved from source pixel to Alpha,
      // Gray value is computed and set to highest word of Pix64 so
      // Pix64.Color contains grayscale value scaled to 64 bits
      Alpha := Pix64.A;
      with GrayConv do
        Pix64.A := Round(R * Pix64.R + G * Pix64.G + B * Pix64.B);

      GraySetDstPixel(Dst, DstInfo, Pix64, Alpha);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure ChannelToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Pix64: TColor64Rec;
  PixF: TColorFPRec;
begin
  for I := 0 to NumPixels - 1 do
  begin
    ChannelGetSrcPixel(Src, SrcInfo, Pix64);

    // floating point channel values are scaled to 1.0
    PixF.A := Pix64.A * OneDiv16Bit;
    PixF.R := Pix64.R * OneDiv16Bit;
    PixF.G := Pix64.G * OneDiv16Bit;
    PixF.B := Pix64.B * OneDiv16Bit;

    FloatSetDstPixel(Dst, DstInfo, PixF);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure ChannelToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);
begin
  ReduceColorsMedianCut(NumPixels, Src, Dst, SrcInfo, DstInfo, DstInfo.PaletteEntries,
    GetOption(ImagingColorReductionMask), DstPal);
end;

procedure GrayToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Gray: TColor64Rec;
  Alpha: Word;
begin
  // two most common conversions (Gray8->Gray16 nad Gray16->Gray8)
  // are made separately from general conversions to make them faster
  if (SrcInfo.Format = ifGray8) and (DstInfo.Format = ifGray16) then
  begin
    for I := 0 to NumPixels - 1 do
      PWordArray(Dst)[I] := PByteArray(Src)[I] shl 8;
  end
  else
  begin
    if (DstInfo.Format = ifGray8) and (SrcInfo.Format = ifGray16) then
    begin
      for I := 0 to NumPixels - 1 do
        PByteArray(Dst)[I] := PWordArray(Src)[I] shr 8;
    end
    else
      for I := 0 to NumPixels - 1 do
      begin
        // general grayscale conversion
        GrayGetSrcPixel(Src, SrcInfo, Gray, Alpha);
        GraySetDstPixel(Dst, DstInfo, Gray, Alpha);
        Inc(Src, SrcInfo.BytesPerPixel);
        Inc(Dst, DstInfo.BytesPerPixel);
      end;
  end;
end;

procedure GrayToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Pix64: TColor64Rec;
  Alpha: Word;
begin
  // two most common conversions (Gray8->R8G8B8 nad Gray8->A8R8G8B8)
  // are made separately from general conversions to make them faster
  if (DstInfo.BytesPerPixel in [3, 4]) and (SrcInfo.Format = ifGray8) then
    for I := 0 to NumPixels - 1 do
    begin
      PColor24Rec(Dst).R := Src^;
      PColor24Rec(Dst).G := Src^;
      PColor24Rec(Dst).B := Src^;
      if DstInfo.HasAlphaChannel then
        PColor32Rec(Dst).A := $FF;
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
    for I := 0 to NumPixels - 1 do
    begin
      GrayGetSrcPixel(Src, SrcInfo, Pix64, Alpha);

      // most significant word of grayscale value is used for
      // each channel and alpha channel is set to Alpha
      Pix64.R := Pix64.A;
      Pix64.G := Pix64.A;
      Pix64.B := Pix64.A;
      Pix64.A := Alpha;

      ChannelSetDstPixel(Dst, DstInfo, Pix64);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure GrayToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Gray: TColor64Rec;
  PixF: TColorFPRec;
  Alpha: Word;
begin
  for I := 0 to NumPixels - 1 do
  begin
    GrayGetSrcPixel(Src, SrcInfo, Gray, Alpha);
    // most significant word of grayscale value is used for
    // each channel and alpha channel is set to Alpha
    // then all is scaled to 0..1
    PixF.R := Gray.A * OneDiv16Bit;
    PixF.G := Gray.A * OneDiv16Bit;
    PixF.B := Gray.A * OneDiv16Bit;
    PixF.A := Alpha * OneDiv16Bit;

    FloatSetDstPixel(Dst, DstInfo, PixF);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure GrayToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);
var
  I: LongInt;
  Idx: UInt32;
  Gray: TColor64Rec;
  Alpha, Shift: Word;
begin
  FillGrayscalePalette(DstPal, DstInfo.PaletteEntries);
  Shift := Log2Int(DstInfo.PaletteEntries);
  // most common conversion (Gray8->Index8)
  // is made separately from general conversions to make it faster
  if (SrcInfo.Format = ifGray8) and (DstInfo.Format = ifIndex8) then
    for I := 0 to NumPixels - 1 do
    begin
      Dst^ := Src^;
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
    for I := 0 to NumPixels - 1 do
    begin
      // gray value is read from src and index to precomputed
      // grayscale palette is computed and written to dst
      // (we assume here that there will be no more than 65536 palette
      // entries in dst format, gray value is shifted so the highest
      // gray value match the highest possible index in palette)
      GrayGetSrcPixel(Src, SrcInfo, Gray, Alpha);
      Idx := Gray.A shr (16 - Shift);
      IndexSetDstPixel(Dst, DstInfo, Idx);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure FloatToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  PixF: TColorFPRec;
begin
  for I := 0 to NumPixels - 1 do
  begin
    // general floating point conversion
    FloatGetSrcPixel(Src, SrcInfo, PixF);
    FloatSetDstPixel(Dst, DstInfo, PixF);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure FloatToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  Pix64: TColor64Rec;
  PixF: TColorFPRec;
begin
  for I := 0 to NumPixels - 1 do
  begin
    FloatGetSrcPixel(Src, SrcInfo, PixF);
    ClampFloatPixel(PixF);

    // floating point channel values are scaled to 1.0
    Pix64.A := ClampToWord(Round(PixF.A * 65535));
    Pix64.R := ClampToWord(Round(PixF.R * 65535));
    Pix64.G := ClampToWord(Round(PixF.G * 65535));
    Pix64.B := ClampToWord(Round(PixF.B * 65535));

    ChannelSetDstPixel(Dst, DstInfo, Pix64);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure FloatToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo);
var
  I: LongInt;
  PixF: TColorFPRec;
  Gray: TColor64Rec;
  Alpha: Word;
begin
  for I := 0 to NumPixels - 1 do
  begin
    FloatGetSrcPixel(Src, SrcInfo, PixF);
    ClampFloatPixel(PixF);

    // alpha is saved from source pixel to Alpha,
    // Gray value is computed and set to highest word of Pix64 so
    // Pix64.Color contains grayscale value scaled to 64 bits
    Alpha := ClampToWord(Round(PixF.A * 65535.0));
    Gray.A := ClampToWord(Round((GrayConv.R * PixF.R + GrayConv.G * PixF.G +
      GrayConv.B * PixF.B) * 65535.0));

    GraySetDstPixel(Dst, DstInfo, Gray, Alpha);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure FloatToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; DstPal: PPalette32);
begin
  ReduceColorsMedianCut(NumPixels, Src, Dst, SrcInfo, DstInfo, DstInfo.PaletteEntries,
    GetOption(ImagingColorReductionMask), DstPal);
end;

procedure IndexToIndex(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal, DstPal: PPalette32);
var
  I: LongInt;
begin
  // there is only one indexed format now, so it is just a copy
  for I := 0 to NumPixels - 1 do
  begin
    Dst^ := Src^;
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
  for I := 0 to SrcInfo.PaletteEntries - 1 do
    DstPal[I] := SrcPal[I];
end;

procedure IndexToChannel(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
var
  I: LongInt;
  Pix64: TColor64Rec;
  Idx: UInt32;
begin
  // two most common conversions (Index8->R8G8B8 nad Index8->A8R8G8B8)
  // are made separately from general conversions to make them faster
  if (SrcInfo.Format = ifIndex8) and (DstInfo.Format in [ifR8G8B8, ifA8R8G8B8]) then
    for I := 0 to NumPixels - 1 do
    begin
      with PColor24Rec(Dst)^ do
      begin
        R := SrcPal[Src^].R;
        G := SrcPal[Src^].G;
        B := SrcPal[Src^].B;
      end;
      if DstInfo.Format = ifA8R8G8B8 then
        PColor32Rec(Dst).A := SrcPal[Src^].A;
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  else
    for I := 0 to NumPixels - 1 do
    begin
      // index to palette is read from source and color
      // is retrieved from palette entry. Color is then
      // scaled to 16bits and written to dest
      IndexGetSrcPixel(Src, SrcInfo, Idx);
      with Pix64 do
      begin
        A := SrcPal[Idx].A shl 8;
        R := SrcPal[Idx].R shl 8;
        G := SrcPal[Idx].G shl 8;
        B := SrcPal[Idx].B shl 8;
      end;
      ChannelSetDstPixel(Dst, DstInfo, Pix64);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure IndexToGray(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
var
  I: LongInt;
  Gray: TColor64Rec;
  Alpha: Word;
  Idx: UInt32;
begin
  // most common conversion (Index8->Gray8)
  // is made separately from general conversions to make it faster
  if (SrcInfo.Format = ifIndex8) and (DstInfo.Format = ifGray8) then
  begin
    for I := 0 to NumPixels - 1 do
    begin
      Dst^ := Round(GrayConv.R * SrcPal[Src^].R + GrayConv.G * SrcPal[Src^].G +
        GrayConv.B * SrcPal[Src^].B);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end
  end
  else
    for I := 0 to NumPixels - 1 do
    begin
      // index to palette is read from source and color
      // is retrieved from palette entry. Color is then
      // transformed to grayscale and assigned to the highest
      // byte of Gray value
      IndexGetSrcPixel(Src, SrcInfo, Idx);
      Alpha := SrcPal[Idx].A shl 8;
      Gray.A := MulDiv(Round(GrayConv.R * SrcPal[Idx].R + GrayConv.G * SrcPal[Idx].G +
        GrayConv.B * SrcPal[Idx].B), 65535, 255);
      GraySetDstPixel(Dst, DstInfo, Gray, Alpha);
      Inc(Src, SrcInfo.BytesPerPixel);
      Inc(Dst, DstInfo.BytesPerPixel);
    end;
end;

procedure IndexToFloat(NumPixels: LongInt; Src, Dst: PByte; SrcInfo,
  DstInfo: PImageFormatInfo; SrcPal: PPalette32);
var
  I: LongInt;
  Idx: UInt32;
  PixF: TColorFPRec;
begin
  for I := 0 to NumPixels - 1 do
  begin
    // index to palette is read from source and color
    // is retrieved from palette entry. Color is then
    // scaled to 0..1 and written to dest
    IndexGetSrcPixel(Src, SrcInfo, Idx);
    with PixF do
    begin
      A := SrcPal[Idx].A * OneDiv8Bit;
      R := SrcPal[Idx].R * OneDiv8Bit;
      G := SrcPal[Idx].G * OneDiv8Bit;
      B := SrcPal[Idx].B * OneDiv8Bit;
    end;
    FloatSetDstPixel(Dst, DstInfo, PixF);
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;


{ Special formats conversion functions }

type
  // DXT RGB color block
  TDXTColorBlock = packed record
    Color0, Color1: Word;
    Mask: UInt32;
  end;
  PDXTColorBlock = ^TDXTColorBlock;

  // DXT explicit alpha for a block
  TDXTAlphaBlockExp = packed record
    Alphas: array[0..3] of Word;
  end;
  PDXTAlphaBlockExp = ^TDXTAlphaBlockExp;

  // DXT interpolated alpha for a block
  TDXTAlphaBlockInt = packed record
    Alphas: array[0..7] of Byte;
  end;
  PDXTAlphaBlockInt = ^TDXTAlphaBlockInt;

  TPixelInfo = record
    Color: Word;
    Alpha: Byte;
    Orig: TColor32Rec;
  end;

  TPixelBlock = array[0..15] of TPixelInfo;

function DecodeCol(Color: Word): TColor32Rec;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  Result.A := $FF;
{  Result.R := ((Color and $F800) shr 11) shl 3;
  Result.G := ((Color and $07E0) shr 5) shl 2;
  Result.B := (Color and $001F) shl 3;}
  // this color expansion is slower but gives better results
  Result.R := (Color shr 11) * 255 div 31;
  Result.G := ((Color shr 5) and $3F) * 255 div 63;
  Result.B := (Color and $1F) * 255 div 31;
end;

procedure DecodeDXT1(SrcBits, DestBits: PByte; Width, Height: LongInt);
var
  Sel, X, Y, I, J, K: LongInt;
  Block: TDXTColorBlock;
  Colors: array[0..3] of TColor32Rec;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      Block := PDXTColorBlock(SrcBits)^;
      Inc(SrcBits, SizeOf(Block));
      // we read and decode endpoint colors
      Colors[0] := DecodeCol(Block.Color0);
      Colors[1] := DecodeCol(Block.Color1);
      // and interpolate between them
      if Block.Color0 > Block.Color1 then
      begin
        // interpolation for block without alpha
        Colors[2].A := $FF;
        Colors[2].R := (Colors[0].R shl 1 + Colors[1].R + 1) div 3;
        Colors[2].G := (Colors[0].G shl 1 + Colors[1].G + 1) div 3;
        Colors[2].B := (Colors[0].B shl 1 + Colors[1].B + 1) div 3;
        Colors[3].A := $FF;
        Colors[3].R := (Colors[0].R + Colors[1].R shl 1 + 1) div 3;
        Colors[3].G := (Colors[0].G + Colors[1].G shl 1 + 1) div 3;
        Colors[3].B := (Colors[0].B + Colors[1].B shl 1 + 1) div 3;
      end
      else
      begin
        // interpolation for block with alpha
        Colors[2].A := $FF;
        Colors[2].R := (Colors[0].R + Colors[1].R) shr 1;
        Colors[2].G := (Colors[0].G + Colors[1].G) shr 1;
        Colors[2].B := (Colors[0].B + Colors[1].B) shr 1;
        Colors[3].A := 0;
        Colors[3].R := (Colors[0].R + Colors[1].R shl 1 + 1) div 3;
        Colors[3].G := (Colors[0].G + Colors[1].G shl 1 + 1) div 3;
        Colors[3].B := (Colors[0].B + Colors[1].B shl 1 + 1) div 3;
      end;

      // we distribute the dxt block colors across the 4x4 block of the
      // destination image according to the dxt block mask
      K := 0;
      for J := 0 to 3 do
        for I := 0 to 3 do
        begin
          Sel := (Block.Mask and (3 shl (K shl 1))) shr (K shl 1);
          if ((X shl 2 + I) < Width) and ((Y shl 2 + J) < Height) then
            PPalette32(DestBits)[(Y shl 2 + J) * Width + X shl 2 + I] :=
              Colors[Sel];
          Inc(K);
        end;
  end;
end;

procedure DecodeDXT3(SrcBits, DestBits: PByte; Width, Height: LongInt);
var
  Sel, X, Y, I, J, K: LongInt;
  Block: TDXTColorBlock;
  AlphaBlock: TDXTAlphaBlockExp;
  Colors: array[0..3] of TColor32Rec;
  AWord: Word;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      AlphaBlock := PDXTAlphaBlockExp(SrcBits)^;
      Inc(SrcBits, SizeOf(AlphaBlock));
      Block := PDXTColorBlock(SrcBits)^;
      Inc(SrcBits, SizeOf(Block));
      // we read and decode endpoint colors
      Colors[0] := DecodeCol(Block.Color0);
      Colors[1] := DecodeCol(Block.Color1);
      // and interpolate between them
      Colors[2].R := (Colors[0].R shl 1 + Colors[1].R + 1) div 3;
      Colors[2].G := (Colors[0].G shl 1 + Colors[1].G + 1) div 3;
      Colors[2].B := (Colors[0].B shl 1 + Colors[1].B + 1) div 3;
      Colors[3].R := (Colors[0].R + Colors[1].R shl 1 + 1) div 3;
      Colors[3].G := (Colors[0].G + Colors[1].G shl 1 + 1) div 3;
      Colors[3].B := (Colors[0].B + Colors[1].B shl 1 + 1) div 3;

      // we distribute the dxt block colors and alphas
      // across the 4x4 block of the destination image
      // according to the dxt block mask and alpha block
      K := 0;
      for J := 0 to 3 do
      begin
        AWord := AlphaBlock.Alphas[J];
        for I := 0 to 3 do
        begin
          Sel := (Block.Mask and (3 shl (K shl 1))) shr (K shl 1);
          if (X shl 2 + I < Width) and (Y shl 2 + J < Height) then
          begin
            Colors[Sel].A := AWord and $0F;
            Colors[Sel].A := Colors[Sel].A or (Colors[Sel].A shl 4);
            PPalette32(DestBits)[(Y shl 2 + J) * Width + X shl 2 + I] :=
              Colors[Sel];
          end;
          Inc(K);
          AWord := AWord shr 4;
        end;
      end;
  end;
end;

procedure GetInterpolatedAlphas(var AlphaBlock: TDXTAlphaBlockInt);
begin
  with AlphaBlock do
  if Alphas[0] > Alphas[1] then
  begin
    // Interpolation of six alphas
    Alphas[2] := (6 * Alphas[0] + 1 * Alphas[1] + 3) div 7;
    Alphas[3] := (5 * Alphas[0] + 2 * Alphas[1] + 3) div 7;
    Alphas[4] := (4 * Alphas[0] + 3 * Alphas[1] + 3) div 7;
    Alphas[5] := (3 * Alphas[0] + 4 * Alphas[1] + 3) div 7;
    Alphas[6] := (2 * Alphas[0] + 5 * Alphas[1] + 3) div 7;
    Alphas[7] := (1 * Alphas[0] + 6 * Alphas[1] + 3) div 7;
  end
  else
  begin
    // Interpolation of four alphas, two alphas are set directly
    Alphas[2] := (4 * Alphas[0] + 1 * Alphas[1] + 2) div 5;
    Alphas[3] := (3 * Alphas[0] + 2 * Alphas[1] + 2) div 5;
    Alphas[4] := (2 * Alphas[0] + 3 * Alphas[1] + 2) div 5;
    Alphas[5] := (1 * Alphas[0] + 4 * Alphas[1] + 2) div 5;
    Alphas[6] := 0;
    Alphas[7] := $FF;
  end;
end;

procedure DecodeDXT5(SrcBits, DestBits: PByte; Width, Height: LongInt);
var
  Sel, X, Y, I, J, K: LongInt;
  Block: TDXTColorBlock;
  AlphaBlock: TDXTAlphaBlockInt;
  Colors: array[0..3] of TColor32Rec;
  AMask: array[0..1] of UInt32;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      AlphaBlock := PDXTAlphaBlockInt(SrcBits)^;
      Inc(SrcBits, SizeOf(AlphaBlock));
      Block := PDXTColorBlock(SrcBits)^;
      Inc(SrcBits, SizeOf(Block));
      // we read and decode endpoint colors
      Colors[0] := DecodeCol(Block.Color0);
      Colors[1] := DecodeCol(Block.Color1);
      // and interpolate between them
      Colors[2].R := (Colors[0].R shl 1 + Colors[1].R + 1) div 3;
      Colors[2].G := (Colors[0].G shl 1 + Colors[1].G + 1) div 3;
      Colors[2].B := (Colors[0].B shl 1 + Colors[1].B + 1) div 3;
      Colors[3].R := (Colors[0].R + Colors[1].R shl 1 + 1) div 3;
      Colors[3].G := (Colors[0].G + Colors[1].G shl 1 + 1) div 3;
      Colors[3].B := (Colors[0].B + Colors[1].B shl 1 + 1) div 3;
      // 6 bit alpha mask is copied into two long words for
      // easier usage
      AMask[0] := PUInt32(@AlphaBlock.Alphas[2])^ and $00FFFFFF;
      AMask[1] := PUInt32(@AlphaBlock.Alphas[5])^ and $00FFFFFF;
      // alpha interpolation between two endpoint alphas
      GetInterpolatedAlphas(AlphaBlock);

      // we distribute the dxt block colors and alphas
      // across the 4x4 block of the destination image
      // accroding to the dxt block mask and alpha block mask
      K := 0;
      for J := 0 to 3 do
        for I := 0 to 3 do
        begin
          Sel := (Block.Mask and (3 shl (K shl 1))) shr (K shl 1);
          if ((X shl 2 + I) < Width) and ((Y shl 2 + J) < Height) then
          begin
            Colors[Sel].A := AlphaBlock.Alphas[AMask[J shr 1] and 7];
            PPalette32(DestBits)[(Y shl 2 + J) * Width + (X shl 2 + I)] :=
              Colors[Sel];
          end;
          Inc(K);
          AMask[J shr 1] := AMask[J shr 1] shr 3;
        end;
  end;
end;

procedure GetBlock(var Block: TPixelBlock; SrcBits: Pointer; XPos, YPos,
  Width, Height: LongInt);
var
  X, Y, I: LongInt;
  Src: PColor32Rec;
begin
  I := 0;
  // 4x4 pixel block is filled with information about every
  // pixel in the block: alpha, original color, 565 color
  for Y := 0 to 3 do
    for X := 0 to 3 do
    begin
      Src := @PPalette32(SrcBits)[(YPos shl 2 + Y) * Width + XPos shl 2 + X];
      Block[I].Color := ((Src.R shr 3) shl 11) or ((Src.G shr 2) shl 5) or
        (Src.B shr 3);
      Block[I].Alpha := Src.A;
      Block[I].Orig := Src^;
      Inc(I);
    end;
end;

function ColorDistance(const C1, C2: TColor32Rec): LongInt;
{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := (C1.R - C2.R) * (C1.R - C2.R) +
    (C1.G - C2.G) * (C1.G - C2.G) + (C1.B - C2.B) * (C1.B - C2.B);
end;

procedure GetEndpoints(const Block: TPixelBlock; var Ep0, Ep1: Word);
var
  I, J, Farthest, Dist: LongInt;
  Colors: array[0..15] of TColor32Rec;
begin
  // we choose two colors from the pixel block which has the
  // largest distance between them
  for I := 0 to 15 do
    Colors[I] := Block[I].Orig;
  Farthest := -1;
  for I := 0 to 15 do
    for J := I + 1 to 15 do
    begin
      Dist := ColorDistance(Colors[I], Colors[J]);
      if Dist > Farthest then
      begin
        Farthest := Dist;
        Ep0 := Block[I].Color;
        Ep1 := Block[J].Color;
      end;
    end;
end;

procedure GetAlphaEndpoints(const Block: TPixelBlock; var Min, Max: Byte);
var
  I: LongInt;
begin
  Min := 255;
  Max := 0;
  // we choose the lowest and the highest alpha values
  for I := 0 to 15 do
  begin
    if Block[I].Alpha < Min then
      Min := Block[I].Alpha;
    if Block[I].Alpha > Max then
      Max := Block[I].Alpha;
  end;
end;

procedure FixEndpoints(var Ep0, Ep1: Word; HasAlpha: Boolean); 
var
  Temp: Word;
begin
  // if dxt block has alpha information, Ep0 must be smaller
  // than Ep1, if the  block has no alpha Ep1 must be smaller
  if HasAlpha then
  begin
    if Ep0 > Ep1 then
    begin
      Temp := Ep0;
      Ep0 := Ep1;
      Ep1 := Temp;
    end;
  end
  else
    if Ep0 < Ep1 then
    begin
      Temp := Ep0;
      Ep0 := Ep1;
      Ep1 := Temp;
    end;
end;

function GetColorMask(Ep0, Ep1: Word; NumCols: LongInt;
  const Block: TPixelBlock): UInt32;
var
  I, J, Closest, Dist: LongInt;
  Colors: array[0..3] of TColor32Rec;
  Mask: array[0..15] of Byte;
begin
  // we decode endpoint colors
  Colors[0] := DecodeCol(Ep0);
  Colors[1] := DecodeCol(Ep1);
  // and interpolate colors between (3 for DXT1 with alpha, 4 for the others)
  if NumCols = 3 then
  begin
    Colors[2].R := (Colors[0].R + Colors[1].R) shr 1;
    Colors[2].G := (Colors[0].G + Colors[1].G) shr 1;
    Colors[2].B := (Colors[0].B + Colors[1].B) shr 1;
    Colors[3].R := (Colors[0].R + Colors[1].R) shr 1;
    Colors[3].G := (Colors[0].G + Colors[1].G) shr 1;
    Colors[3].B := (Colors[0].B + Colors[1].B) shr 1;
  end
  else
  begin
    Colors[2].R := (Colors[0].R shl 1 + Colors[1].R + 1) div 3;
    Colors[2].G := (Colors[0].G shl 1 + Colors[1].G + 1) div 3;
    Colors[2].B := (Colors[0].B shl 1 + Colors[1].B + 1) div 3;
    Colors[3].R := (Colors[0].R + Colors[1].R shl 1 + 1) div 3;
    Colors[3].G := (Colors[0].G + Colors[1].G shl 1 + 1) div 3;
    Colors[3].B := (Colors[0].B + Colors[1].B shl 1 + 1) div 3;
  end;

  for I := 0 to 15 do
  begin
    // this is only for DXT1 with alpha
    if (Block[I].Alpha < 128) and (NumCols = 3) then
    begin
      Mask[I] := 3;
      Continue;
    end;
    // for each of the 16 input pixels the nearest color in the
    // 4 dxt colors is found
    Closest := MaxInt;
    for J := 0 to NumCols - 1 do
    begin
      Dist := ColorDistance(Block[I].Orig, Colors[J]);
      if Dist < Closest then
      begin
        Closest := Dist;
        Mask[I] := J;
      end;
    end;
  end;

  Result := 0;
  for I := 0 to 15 do
    Result := Result or (Mask[I] shl (I shl 1));
end;

procedure GetAlphaMask(Ep0, Ep1: Byte; var Block: TPixelBlock; Mask: PByteArray);
var
  Alphas: array[0..7] of Byte;
  M: array[0..15] of Byte;
  I, J, Closest, Dist: LongInt;
begin
  Alphas[0] := Ep0;
  Alphas[1] := Ep1;
  // interpolation between two given alpha endpoints
  // (I use 6 interpolated values mode)
  Alphas[2] := (6 * Alphas[0] + 1 * Alphas[1] + 3) div 7;
  Alphas[3] := (5 * Alphas[0] + 2 * Alphas[1] + 3) div 7;
  Alphas[4] := (4 * Alphas[0] + 3 * Alphas[1] + 3) div 7;
  Alphas[5] := (3 * Alphas[0] + 4 * Alphas[1] + 3) div 7;
  Alphas[6] := (2 * Alphas[0] + 5 * Alphas[1] + 3) div 7;
  Alphas[7] := (1 * Alphas[0] + 6 * Alphas[1] + 3) div 7;

  // the closest interpolated values for each of the input alpha
  // is found
  for I := 0 to 15 do
  begin
    Closest := MaxInt;
    for J := 0 to 7 do
    begin
      Dist := Abs(Alphas[J] - Block[I].Alpha);
      if Dist < Closest then
      begin
        Closest := Dist;
        M[I] := J;
      end;
    end;
  end;

  Mask[0] := M[0] or (M[1] shl 3) or ((M[2] and 3) shl 6);
  Mask[1] := ((M[2] and 4) shr 2) or (M[3] shl 1) or (M[4] shl 4) or
    ((M[5] and 1) shl 7);
  Mask[2] := ((M[5] and 6) shr 1) or (M[6] shl 2) or (M[7] shl 5);
  Mask[3] := M[8] or (M[9] shl 3) or ((M[10] and 3) shl 6);
  Mask[4] := ((M[10] and 4) shr 2) or (M[11] shl 1) or (M[12] shl 4) or
   ((M[13] and 1) shl 7);
  Mask[5] := ((M[13] and 6) shr 1) or (M[14] shl 2) or (M[15] shl 5);
end;


procedure EncodeDXT1(SrcBits: PByte; DestBits: PByte; Width, Height: LongInt);
var
  X, Y, I: LongInt;
  HasAlpha: Boolean;
  Block: TDXTColorBlock;
  Pixels: TPixelBlock;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      GetBlock(Pixels, SrcBits, X, Y, Width, Height);
      HasAlpha := False;
      for I := 0 to 15 do
        if Pixels[I].Alpha < 128 then
        begin
          HasAlpha := True;
          Break;
        end;
      GetEndpoints(Pixels, Block.Color0, Block.Color1);
      FixEndpoints(Block.Color0, Block.Color1, HasAlpha);
      if HasAlpha then
        Block.Mask := GetColorMask(Block.Color0, Block.Color1, 3, Pixels)
      else
        Block.Mask := GetColorMask(Block.Color0, Block.Color1, 4, Pixels);
      PDXTColorBlock(DestBits)^ := Block;
      Inc(DestBits, SizeOf(Block));
    end;
end;

procedure EncodeDXT3(SrcBits: Pointer; DestBits: PByte; Width, Height: LongInt);
var
  X, Y, I: LongInt;
  Block: TDXTColorBlock;
  AlphaBlock: TDXTAlphaBlockExp;
  Pixels: TPixelBlock;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      GetBlock(Pixels, SrcBits, X, Y, Width, Height);
      for I := 0 to 7 do
        PByteArray(@AlphaBlock.Alphas)[I] :=
          (Pixels[I shl 1].Alpha shr 4) or ((Pixels[I shl 1 + 1].Alpha shr 4) shl 4);
      GetEndpoints(Pixels, Block.Color0, Block.Color1);
      FixEndpoints(Block.Color0, Block.Color1, False);
      Block.Mask := GetColorMask(Block.Color0, Block.Color1, 4, Pixels);
      PDXTAlphaBlockExp(DestBits)^ := AlphaBlock;
      Inc(DestBits, SizeOf(AlphaBlock));
      PDXTColorBlock(DestBits)^ := Block;
      Inc(DestBits, SizeOf(Block));
    end;
end;

procedure EncodeDXT5(SrcBits: Pointer; DestBits: PByte; Width, Height: LongInt);
var
  X, Y: LongInt;
  Block: TDXTColorBlock;
  AlphaBlock: TDXTAlphaBlockInt;
  Pixels: TPixelBlock;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      GetBlock(Pixels, SrcBits, X, Y, Width, Height);
      GetEndpoints(Pixels, Block.Color0, Block.Color1);
      FixEndpoints(Block.Color0, Block.Color1, False);
      Block.Mask := GetColorMask(Block.Color0, Block.Color1, 4, Pixels);
      GetAlphaEndPoints(Pixels, AlphaBlock.Alphas[1], AlphaBlock.Alphas[0]);
      GetAlphaMask(AlphaBlock.Alphas[0], AlphaBlock.Alphas[1], Pixels,
        PByteArray(@AlphaBlock.Alphas[2]));
      PDXTAlphaBlockInt(DestBits)^ := AlphaBlock;
      Inc(DestBits, SizeOf(AlphaBlock));
      PDXTColorBlock(DestBits)^ := Block;
      Inc(DestBits, SizeOf(Block));
    end;
end;

type
  TBTCBlock = packed record
    MLower, MUpper: Byte;
    BitField: Word;
  end;
  PBTCBlock = ^TBTCBlock;

procedure EncodeBTC(SrcBits: Pointer; DestBits: PByte; Width, Height: Integer);
var
  X, Y, I, J: Integer;
  Block: TBTCBlock;
  M, MLower, MUpper, K: Integer;
  Pixels: array[0..15] of Byte;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      M := 0;
      MLower := 0;
      MUpper := 0;
      FillChar(Block, SizeOf(Block), 0);
      K := 0;

      // Store 4x4 pixels and compute average, lower, and upper intensity levels
      for I := 0 to 3 do
        for J := 0 to 3 do
        begin
          Pixels[K] := PByteArray(SrcBits)[(Y shl 2 + I) * Width + X shl 2 + J];
          Inc(M, Pixels[K]);
          Inc(K);
        end;

      M := M div 16;
      K := 0;

      // Now compute upper and lower levels, number of upper pixels,
      // and update bit field (1 when pixel is above avg. level M)
      for I := 0 to 15 do
      begin
        if Pixels[I] > M then
        begin
          Inc(MUpper, Pixels[I]);
          Inc(K);
          Block.BitField := Block.BitField or (1 shl I);
        end
        else
          Inc(MLower, Pixels[I]);
      end;

      // Scale levels and save them to block
      if K > 0 then
        Block.MUpper := ClampToByte(MUpper div K)
      else
        Block.MUpper := 0;
      Block.MLower := ClampToByte(MLower div (16 - K));

      // Finally save block to dest data
      PBTCBlock(DestBits)^ := Block;
      Inc(DestBits, SizeOf(Block));
    end;
end;

procedure GetOneChannelBlock(var Block: TPixelBlock; SrcBits: Pointer; XPos, YPos,
  Width, Height, BytesPP, ChannelIdx: Integer);
var
  X, Y, I: Integer;
  Src: PByte;
begin
  I := 0;
  // 4x4 pixel block is filled with information about every pixel in the block,
  // but only one channel value is stored in Alpha field
  for Y := 0 to 3 do
    for X := 0 to 3 do
    begin
      Src := @PByteArray(SrcBits)[(YPos * 4 + Y) * Width * BytesPP +
        (XPos * 4 + X) * BytesPP + ChannelIdx];
      Block[I].Alpha := Src^;
      Inc(I);
    end;
end;

procedure EncodeATI1N(SrcBits: Pointer; DestBits: PByte; Width, Height: Integer);
var
  X, Y: Integer;
  AlphaBlock: TDXTAlphaBlockInt;
  Pixels: TPixelBlock;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      // Encode one channel
      GetOneChannelBlock(Pixels, SrcBits, X, Y, Width, Height, 1, 0);
      GetAlphaEndPoints(Pixels, AlphaBlock.Alphas[1], AlphaBlock.Alphas[0]);
      GetAlphaMask(AlphaBlock.Alphas[0], AlphaBlock.Alphas[1], Pixels,
        PByteArray(@AlphaBlock.Alphas[2]));
      PDXTAlphaBlockInt(DestBits)^ := AlphaBlock;
      Inc(DestBits, SizeOf(AlphaBlock));
    end;
end;

procedure EncodeATI2N(SrcBits: Pointer; DestBits: PByte; Width, Height: Integer);
var
  X, Y: Integer;
  AlphaBlock: TDXTAlphaBlockInt;
  Pixels: TPixelBlock;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      // Encode Red/X channel
      GetOneChannelBlock(Pixels, SrcBits, X, Y, Width, Height, 4, ChannelRed);
      GetAlphaEndPoints(Pixels, AlphaBlock.Alphas[1], AlphaBlock.Alphas[0]);
      GetAlphaMask(AlphaBlock.Alphas[0], AlphaBlock.Alphas[1], Pixels,
        PByteArray(@AlphaBlock.Alphas[2]));
      PDXTAlphaBlockInt(DestBits)^ := AlphaBlock;
      Inc(DestBits, SizeOf(AlphaBlock));
      // Encode Green/Y channel
      GetOneChannelBlock(Pixels, SrcBits, X, Y, Width, Height, 4, ChannelGreen);
      GetAlphaEndPoints(Pixels, AlphaBlock.Alphas[1], AlphaBlock.Alphas[0]);
      GetAlphaMask(AlphaBlock.Alphas[0], AlphaBlock.Alphas[1], Pixels,
        PByteArray(@AlphaBlock.Alphas[2]));
      PDXTAlphaBlockInt(DestBits)^ := AlphaBlock;
      Inc(DestBits, SizeOf(AlphaBlock));
    end;
end;

procedure EncodeBinary(SrcBits: Pointer; DestBits: PByte; Width, Height: Integer);
var
  Src: PByte absolute SrcBits;
  Bitmap: PByteArray absolute DestBits;
  X, Y, WidthBytes: Integer;
  PixelThresholded, Threshold: Byte;
begin
  Threshold := ClampToByte(GetOption(ImagingBinaryThreshold));
  WidthBytes := (Width + 7) div 8;

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      if Src^ > Threshold then
        PixelThresholded := 255
      else
        PixelThresholded := 0;

      Bitmap[Y * WidthBytes + X div 8] := Bitmap[Y * WidthBytes + X div 8] or // OR current value of byte with following:
        (PixelThresholded and 1)  // To make 1 from 255, 0 remains 0
        shl (7 - (X mod 8));  // Put current bit to proper place in byte

      Inc(Src);
    end;
end;

procedure DecodeBTC(SrcBits, DestBits: PByte; Width, Height: Integer);
var
  X, Y, I, J, K: Integer;
  Block: TBTCBlock;
  Dest: PByte;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      Block := PBTCBlock(SrcBits)^;
      Inc(SrcBits, SizeOf(Block));
      K := 0;

      // Just write MUpper when there is '1' in bit field and MLower
      // when there is '0'
      for I := 0 to 3 do
        for J := 0 to 3 do
        begin
          Dest := @PByteArray(DestBits)[(Y shl 2 + I) * Width + X shl 2 + J];
          if Block.BitField and (1 shl K) <> 0 then
            Dest^ := Block.MUpper
          else
            Dest^ := Block.MLower;
          Inc(K);
        end;
    end;
end;

procedure DecodeATI1N(SrcBits, DestBits: PByte; Width, Height: Integer);
var
  X, Y, I, J: Integer;
  AlphaBlock: TDXTAlphaBlockInt;
  AMask: array[0..1] of UInt32;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      AlphaBlock := PDXTAlphaBlockInt(SrcBits)^;
      Inc(SrcBits, SizeOf(AlphaBlock));
      // 6 bit alpha mask is copied into two long words for
      // easier usage
      AMask[0] := PUInt32(@AlphaBlock.Alphas[2])^ and $00FFFFFF;
      AMask[1] := PUInt32(@AlphaBlock.Alphas[5])^ and $00FFFFFF;
      // alpha interpolation between two endpoint alphas
      GetInterpolatedAlphas(AlphaBlock);

      // we distribute the dxt block alphas
      // across the 4x4 block of the destination image
      for J := 0 to 3 do
       for I := 0 to 3 do
       begin
         PByteArray(DestBits)[(Y shl 2 + J) * Width + (X shl 2 + I)] :=
           AlphaBlock.Alphas[AMask[J shr 1] and 7];
         AMask[J shr 1] := AMask[J shr 1] shr 3;
       end;
  end;
end;

procedure DecodeATI2N(SrcBits, DestBits: PByte; Width, Height: Integer);
var
  X, Y, I, J: Integer;
  Color: TColor32Rec;
  AlphaBlock1, AlphaBlock2: TDXTAlphaBlockInt;
  AMask1: array[0..1] of UInt32;
  AMask2: array[0..1] of UInt32;
begin
  for Y := 0 to Height div 4 - 1 do
    for X := 0 to Width div 4 - 1 do
    begin
      // Read the first alpha block and get masks
      AlphaBlock1 := PDXTAlphaBlockInt(SrcBits)^;
      Inc(SrcBits, SizeOf(AlphaBlock1));
      AMask1[0] := PUInt32(@AlphaBlock1.Alphas[2])^ and $00FFFFFF;
      AMask1[1] := PUInt32(@AlphaBlock1.Alphas[5])^ and $00FFFFFF;
      // Read the secind alpha block and get masks
      AlphaBlock2 := PDXTAlphaBlockInt(SrcBits)^;
      Inc(SrcBits, SizeOf(AlphaBlock2));
      AMask2[0] := PUInt32(@AlphaBlock2.Alphas[2])^ and $00FFFFFF;
      AMask2[1] := PUInt32(@AlphaBlock2.Alphas[5])^ and $00FFFFFF;
      // alpha interpolation between two endpoint alphas
      GetInterpolatedAlphas(AlphaBlock1);
      GetInterpolatedAlphas(AlphaBlock2);

      Color.A := $FF;
      Color.B := 0;

      // Distribute alpha block values across 4x4 pixel block,
      // first alpha block represents Red channel, second is Green. 
      for J := 0 to 3 do
       for I := 0 to 3 do
       begin
         Color.R := AlphaBlock1.Alphas[AMask1[J shr 1] and 7];
         Color.G := AlphaBlock2.Alphas[AMask2[J shr 1] and 7];
         PColor32RecArray(DestBits)[(Y shl 2 + J) * Width + (X shl 2 + I)] := Color;
         AMask1[J shr 1] := AMask1[J shr 1] shr 3;
         AMask2[J shr 1] := AMask2[J shr 1] shr 3;
       end;
  end;
end;

procedure DecodeBinary(SrcBits, DestBits: PByte; Width, Height: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Convert1To8(SrcBits, DestBits, Width, Height, (Width + 7) div 8, True);
end;

procedure SpecialToUnSpecial(const SrcImage: TImageData; DestBits: Pointer;
  SpecialFormat: TImageFormat);
begin
  case SpecialFormat of
    ifDXT1: DecodeDXT1(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifDXT3: DecodeDXT3(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifDXT5: DecodeDXT5(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifBTC:  DecodeBTC (SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifATI1N: DecodeATI1N(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifATI2N: DecodeATI2N(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
    ifBinary: DecodeBinary(SrcImage.Bits, DestBits, SrcImage.Width, SrcImage.Height);
  end;
end;

procedure UnSpecialToSpecial(SrcBits: Pointer; const DestImage: TImageData;
  SpecialFormat: TImageFormat);
begin
  case SpecialFormat of
    ifDXT1: EncodeDXT1(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifDXT3: EncodeDXT3(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifDXT5: EncodeDXT5(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifBTC:  EncodeBTC (SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifATI1N: EncodeATI1N(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifATI2N: EncodeATI2N(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
    ifBinary: EncodeBinary(SrcBits, DestImage.Bits, DestImage.Width, DestImage.Height);
  end;
end;

procedure ConvertSpecial(var Image: TImageData;
  SrcInfo, DstInfo: PImageFormatInfo);
var
  WorkImage: TImageData;

  procedure CheckSize(var Img: TImageData; Info: PImageFormatInfo);
  var
    Width, Height: LongInt;
  begin
    Width := Img.Width;
    Height := Img.Height;
    DstInfo.CheckDimensions(Info.Format, Width, Height);
    ResizeImage(Img, Width, Height, rfNearest);
  end;

begin
  if SrcInfo.IsSpecial and DstInfo.IsSpecial then
  begin
    // Convert source to nearest 'normal' format
    InitImage(WorkImage);
    NewImage(Image.Width, Image.Height, SrcInfo.SpecialNearestFormat, WorkImage);
    SpecialToUnSpecial(Image, WorkImage.Bits, SrcInfo.Format);
    FreeImage(Image);
    // Make sure output of SpecialToUnSpecial is the same as input of
    // UnSpecialToSpecial
    if SrcInfo.SpecialNearestFormat <> DstInfo.SpecialNearestFormat then
      ConvertImage(WorkImage, DstInfo.SpecialNearestFormat);
    // Convert work image to dest special format
    CheckSize(WorkImage, DstInfo);
    NewImage(WorkImage.Width, WorkImage.Height, DstInfo.Format, Image);
    UnSpecialToSpecial(WorkImage.Bits, Image, DstInfo.Format);
    FreeImage(WorkImage);
  end
  else if SrcInfo.IsSpecial and not DstInfo.IsSpecial then
  begin
    // Convert source to nearest 'normal' format
    InitImage(WorkImage);
    NewImage(Image.Width, Image.Height, SrcInfo.SpecialNearestFormat, WorkImage);
    SpecialToUnSpecial(Image, WorkImage.Bits, SrcInfo.Format);
    FreeImage(Image);
    // Now convert to dest format
    ConvertImage(WorkImage, DstInfo.Format);
    Image := WorkImage;
  end
  else if not SrcInfo.IsSpecial and DstInfo.IsSpecial then
  begin
    // Convert source to nearest format
    WorkImage := Image;
    ConvertImage(WorkImage, DstInfo.SpecialNearestFormat);
    // Now convert from nearest to dest
    CheckSize(WorkImage, DstInfo);
    InitImage(Image);
    NewImage(WorkImage.Width, WorkImage.Height, DstInfo.Format, Image);
    UnSpecialToSpecial(WorkImage.Bits, Image, DstInfo.Format);
    FreeImage(WorkImage);
  end;
end;

function GetStdPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  if FInfos[Format] <> nil then
    Result := Width * Height * FInfos[Format].BytesPerPixel
  else
    Result := 0;
end;

procedure CheckStdDimensions(Format: TImageFormat; var Width, Height: LongInt);
begin
end;

function GetDXTPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  // DXT can be used only for images with dimensions that are
  // multiples of four
  CheckDXTDimensions(Format, Width, Height);
  Result := Width * Height;
  if Format in [ifDXT1, ifATI1N] then
    Result := Result div 2;
end;

procedure CheckDXTDimensions(Format: TImageFormat; var Width, Height: LongInt);
begin
  // DXT image dimensions must be multiples of four
  Width := (Width + 3) and not 3; // div 4 * 4;
  Height := (Height + 3) and not 3; // div 4 * 4;
end;

function GetBTCPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  // BTC can be used only for images with dimensions that are
  // multiples of four
  CheckDXTDimensions(Format, Width, Height);
  Result := Width * Height div 4; // 2bits/pixel
end;

// Castle Game Engine added:
{$ifdef FPC} {$push} {$warnings off} {$endif} // avoid Warning: Function result does not seem to be set
function GetBCPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  raise ENotImplemented.Create();
end;
{$ifdef FPC} {$pop} {$endif}

procedure CheckBCDimensions(Format: TImageFormat; var Width, Height: LongInt);
begin
  raise ENotImplemented.Create();
end;

function GetBinaryPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  // Binary images are aligned on BYTE boundary
  Result := ((Width + 7) div 8) * Height; // 1bit/pixel
end;

{ Optimized pixel readers/writers for 32bit and FP colors to be stored in TImageFormatInfo }

function GetPixel32ifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColor32Rec;
begin
  Result.Color := PUInt32(Bits)^;
end;

procedure SetPixel32ifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColor32Rec);
begin
  PUInt32(Bits)^ := Color.Color;
end;

function GetPixelFPifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec;
begin
  Result.A := PColor32Rec(Bits).A * OneDiv8Bit;
  Result.R := PColor32Rec(Bits).R * OneDiv8Bit;
  Result.G := PColor32Rec(Bits).G * OneDiv8Bit;
  Result.B := PColor32Rec(Bits).B * OneDiv8Bit;
end;

procedure SetPixelFPifA8R8G8B8(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec);
begin
  PColor32Rec(Bits).A := ClampToByte(Round(Color.A * 255.0));
  PColor32Rec(Bits).R := ClampToByte(Round(Color.R * 255.0));
  PColor32Rec(Bits).G := ClampToByte(Round(Color.G * 255.0));
  PColor32Rec(Bits).B := ClampToByte(Round(Color.B * 255.0));
end;

function GetPixel32Channel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColor32Rec;
begin
  case Info.Format of
    ifR8G8B8, ifX8R8G8B8:
      begin
        Result.A := $FF;
        PColor24Rec(@Result)^ := PColor24Rec(Bits)^;
      end;
    ifGray8, ifA8Gray8:
      begin
        if Info.HasAlphaChannel then
          Result.A := PWordRec(Bits).High
        else
          Result.A := $FF;
        Result.R := PWordRec(Bits).Low;
        Result.G := PWordRec(Bits).Low;
        Result.B := PWordRec(Bits).Low;
      end;
  end;
end;

procedure SetPixel32Channel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColor32Rec);
begin
  case Info.Format of
    ifR8G8B8, ifX8R8G8B8:
      begin
        PColor24Rec(Bits)^ := PColor24Rec(@Color)^;
      end;
    ifGray8, ifA8Gray8:
      begin
        if Info.HasAlphaChannel then
          PWordRec(Bits).High := Color.A;
        PWordRec(Bits).Low := Round(GrayConv.R * Color.R + GrayConv.G * Color.G +
          GrayConv.B * Color.B);
      end;
  end;
end;

function GetPixelFPChannel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec;
begin
  case Info.Format of
    ifR8G8B8, ifX8R8G8B8:
      begin
        Result.A := 1.0;
        Result.R := PColor24Rec(Bits).R * OneDiv8Bit;
        Result.G := PColor24Rec(Bits).G * OneDiv8Bit;
        Result.B := PColor24Rec(Bits).B * OneDiv8Bit;
      end;
    ifGray8, ifA8Gray8:
      begin
        if Info.HasAlphaChannel then
          Result.A := PWordRec(Bits).High * OneDiv8Bit
        else
          Result.A := 1.0;
        Result.R := PWordRec(Bits).Low * OneDiv8Bit;
        Result.G := PWordRec(Bits).Low * OneDiv8Bit;
        Result.B := PWordRec(Bits).Low * OneDiv8Bit;
      end;
  end;
end;

procedure SetPixelFPChannel8Bit(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec);
begin
  case Info.Format of
    ifR8G8B8, ifX8R8G8B8:
      begin
        PColor24Rec(Bits).R := ClampToByte(Round(Color.R * 255.0));
        PColor24Rec(Bits).G := ClampToByte(Round(Color.G * 255.0));
        PColor24Rec(Bits).B := ClampToByte(Round(Color.B * 255.0));
      end;
    ifGray8, ifA8Gray8:
      begin
        if Info.HasAlphaChannel then
          PWordRec(Bits).High := ClampToByte(Round(Color.A * 255.0));
        PWordRec(Bits).Low := ClampToByte(Round((GrayConv.R * Color.R + GrayConv.G * Color.G +
          GrayConv.B * Color.B) * 255.0));
      end;
  end;
end;

function GetPixelFPFloat32(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32): TColorFPRec;
begin
  case Info.Format of
    ifA32R32G32B32F, ifA32B32G32R32F:
      begin
        Result := PColorFPRec(Bits)^;
      end;
    ifR32G32B32F, ifB32G32R32F:
      begin
        Result.A := 1.0;
        Result.Color96Rec := PColor96FPRec(Bits)^;
      end;
    ifR32F:
      begin
        Result.A := 1.0;
        Result.R := PSingle(Bits)^;
        Result.G := 0.0;
        Result.B := 0.0;
      end;
  end;
  if Info.IsRBSwapped then
    SwapValues(Result.R, Result.B);
end;

procedure SetPixelFPFloat32(Bits: Pointer; Info: PImageFormatInfo; Palette: PPalette32; const Color: TColorFPRec);
begin
  case Info.Format of
    ifA32R32G32B32F, ifA32B32G32R32F:
      begin
        PColorFPRec(Bits)^ := Color;
      end;
    ifR32G32B32F, ifB32G32R32F:
      begin
        PColor96FPRec(Bits)^ := Color.Color96Rec;
      end;
    ifR32F:
      begin
        PSingle(Bits)^ := Color.R;
      end;
  end;
  if Info.IsRBSwapped then
    SwapValues(PColor96FPRec(Bits).R, PColor96FPRec(Bits).B);
end;

initialization
  // Initialize default sampling filter function pointers and radii
  SamplingFilterFunctions[sfNearest]    := FilterNearest;
  SamplingFilterFunctions[sfLinear]     := FilterLinear;
  SamplingFilterFunctions[sfCosine]     := FilterCosine;
  SamplingFilterFunctions[sfHermite]    := FilterHermite;
  SamplingFilterFunctions[sfQuadratic]  := FilterQuadratic;
  SamplingFilterFunctions[sfGaussian]   := FilterGaussian;
  SamplingFilterFunctions[sfSpline]     := FilterSpline;
  SamplingFilterFunctions[sfLanczos]    := FilterLanczos;
  SamplingFilterFunctions[sfMitchell]   := FilterMitchell;
  SamplingFilterFunctions[sfCatmullRom] := FilterCatmullRom;
  SamplingFilterRadii[sfNearest]    := 1.0;
  SamplingFilterRadii[sfLinear]     := 1.0;
  SamplingFilterRadii[sfCosine]     := 1.0;
  SamplingFilterRadii[sfHermite]    := 1.0;
  SamplingFilterRadii[sfQuadratic]  := 1.5;
  SamplingFilterRadii[sfGaussian]   := 1.25;
  SamplingFilterRadii[sfSpline]     := 2.0;
  SamplingFilterRadii[sfLanczos]    := 3.0;
  SamplingFilterRadii[sfMitchell]   := 2.0;
  SamplingFilterRadii[sfCatmullRom] := 2.0;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.80 -------------------------------------------------------
    - Added PaletteIsGrayScale and Color32ToGray functions.

  -- 0.77 Changes/Bug Fixes -------------------------------------
    - NOT YET: Added support for Passthrough image data formats.
    - Added ConvertToPixel32 helper function.

  -- 0.26.5 Changes/Bug Fixes -----------------------------------
    - Removed optimized codepath for few data formats from StretchResample
      function. It was quite buggy and not so much faster anyway.
    - Added PaletteHasAlpha function.
    - Added support functions for ifBinary data format.
    - Added optional pixel scaling to Convert1To8, Convert2To8,
      abd Convert4To8 functions.

  -- 0.26.3 Changes/Bug Fixes -----------------------------------
    - Filtered resampling ~10% faster now.
    - Fixed DXT3 alpha encoding.
    - ifIndex8 format now has HasAlphaChannel=True.

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Made some resampling stuff public so that it can be used in canvas class.
    - Added some color constructors.
    - Added VisualizePalette helper function.
    - Fixed ConvertSpecial, not very readable before and error when
      converting special->special.

  -- 0.24.3 Changes/Bug Fixes -----------------------------------
    - Some refactorings a changes to DXT based formats.
    - Added ifATI1N and ifATI2N image data formats support structures and functions.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added ifBTC image format support structures and functions.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - FillMipMapLevel now works well with indexed and special formats too.
    - Moved Convert1To8 and Convert4To8 functions from ImagingBitmaps here
     and created new Convert2To8 function. They are now used by more than one
     file format loader. 

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - StretchResample now uses pixel get/set functions stored in
      TImageFormatInfo so it is  much faster for formats that override
      them with optimized ones
    - added pixel set/get functions optimized for various image formats
      (to be stored in TImageFormatInfo)
    - bug in ConvertSpecial caused problems when converting DXTC images
      to bitmaps in ImagingComponents
    - bug in StretchRect caused that it didn't work with ifR32F and
      ifR16F formats
    - removed leftover code in FillMipMapLevel which disabled
      filtered resizing of images witch ChannelSize <> 8bits
    - added half float converting functions and support for half based
      image formats where needed
    - added TranslatePixel and IsImageFormatValid functions
    - fixed possible range overflows when converting from FP to integer images
    - added pixel set/get functions: GetPixel32Generic, GetPixelFPGeneric,
      SetPixel32Generic, SetPixelFPGeneric
    - fixed occasional range overflows in StretchResample

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added StretchNearest, StretchResample and some sampling functions
    - added ChannelCount values to TImageFormatInfo constants
    - added resolution validity check to GetDXTPixelsSize

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - added RBSwapFormat values to some TImageFormatInfo definitions
    - fixed bug in ConvertSpecial (causing DXT images to convert only to 32bit)
    - added CopyPixel, ComparePixels helper functions

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - replaced pixel format conversions for colors not to be
      darkened when converting from low bit counts
    - ReduceColorsMedianCut was updated to support creating one
      optimal palette for more images and it is somewhat faster
      now too
    - there was ugly bug in DXTC dimensions checking
}

end.

