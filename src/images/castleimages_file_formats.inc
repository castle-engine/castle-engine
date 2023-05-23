{%MainUnit castleimages.pas}
{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Image formats. }

type
  { Possible image file format.

    The order on this list matters --- it determines the order of filters
    for open/save dialogs.
    First we list most adviced and well-known formats, starting from lossless. }
  TImageFormat = (
    { We have full PNG support, for both reading and writing.
      We can use LibPng (if available on given system), (FPC) FpImage,
      Vampyre Imaging Library, or (Delphi) PngImage. }
    ifPNG,

    { Image formats below are supported by FPImage and/or Vampyre Imaging Library. }
    ifBMP,
    ifJPEG,
    ifGIF,
    ifTGA,
    ifXPM,
    ifPSD,
    ifPCX,
    ifPNM,

    { We handle fully DDS (DirectDraw Surface) and KTX image formats.
      They provide images in formats optimal for GPU. }
    ifDDS,
    ifKTX,

    { Reading ASTC image format }
    ifASTC,

    { High-dynamic range image format, originally used by Radiance.
      See e.g. the pfilt and ximage programs from the Radiance package
      for processing such images.

      The float color values are encoded smartly as 4 bytes:
      3 mantisas for RGB and 1 byte for an Exponent.
      This is the Greg Ward's RGBE color encoding described in the
      "Graphic Gems" (gem II.5). This allows high floating-point-like precision,
      and possibility to encode any value >= 0 (not necessarily <= 1),
      keeping the pixel only 4 bytes long.

      Encoding a color values with float precision is very useful.
      Otherwise, when synthesized / photographed images are
      very dark / very bright, simply encoding them in traditional fixed-point
      pixel format looses color precision. So potentially important but small
      differences are lost in fixed-point formats.
      And color values are clamped to [0..1] range.
      On the other hand, keeping colors as floats preserves
      everything, and allows to process images later.

      It's most useful and natural to load/save these files as TRGBFloatImage,
      this way you keep the floating-point precision inside memory.
      However, you can also load/convert such image format
      to normal 8-bits image formats (like TRGBImage),
      if you're Ok with losing some of the precision. }
    ifRGBE,

    ifIPL

    {$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
    ,
    ifTIFF,
    ifJP2
    {$endif}
  );
  TImageFormats = set of TImageFormat;

{ Loading image (format-specific) ---------------------------------------

  Load image from Stream.

  Image loaders may look at AllowedImageClasses (to return a data type matching required
  AllowedImageClasses) but they don't have to. A wrapper (LoadEncodedImage) will always
  fix the class to satisfy AllowedImageClasses anyway (albeit doing it inside the particular
  loader may be faster, e.g. it's better to use LibPng transformations than LoadEncodedImage
  FixImageClass, as it avoids unnecessarily copying the data is wrong format, only to fix it later.)

  An appropriate descendant of EImageLoadError will be raised
  in case of error when reading from Stream or when Stream will not
  contain correct data. }

type
  EInvalidBMP = class(EInvalidImageFormat);
  EInvalidPNG = class(EInvalidImageFormat);
  EInvalidIPL = class(EInvalidImageFormat);
  EInvalidASTC = class(EInvalidImageFormat);

function LoadPNG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadBMP(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadGIF(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadTGA(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{function LoadSGI(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;}

{$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
function LoadTIFF(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;
function LoadJP2(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;
{$endif}

{function LoadEXR(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;}

function LoadJPEG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadXPM(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadPSD(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadPCX(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load PNM image (PNM, PGM, PBM, PPM). }
function LoadPNM(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadIPL(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load RGBE image. }
function LoadRGBE(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load DDS image file into a single 2D image. This simply returns the first
  image found in DDS file, which should be the main image.
  If you want to investigate other images in DDS, you have to use TCompositeImage
  class. }
function LoadDDS(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;
function LoadKTX(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadASTC(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Saving image (format-specific) --------------------------------------------

  SaveXxx. Each file format may have specialized SaveXxx that allows
  you to give some parameters special for given format.

  Each format must also have procedure with two parameters
  (Img: TEncodedImage; Stream: TStream), this will be used with
  ImageFormatsInfo[].
  This means that below we must use overloading instead of
  default parameters, since pointers to given procedures must be
  compatible with @link(TImageSaveFunc).

  SaveXxx should
    raise EImageSaveError.CreateFmt('Saving to XXX image class %s not possible', [Img.ClassName]);
  when Img doesn't have acceptable class.
  Also, list of handled image classes should be reflected in SavedClasses
  in ImageFormatsInfo[] for this format.
}

{ }
procedure SaveBMP(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SavePNG(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveGIF(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveTGA(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveXPM(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SavePNM(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveJPEG(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveRGBE(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveDDS(const Img: TEncodedImage; const Stream: TStream); forward;
{$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
procedure SaveTIFF(const Img: TEncodedImage; const Stream: TStream); forward;
procedure SaveJP2(const Img: TEncodedImage; const Stream: TStream); forward;
{$endif}

{ Detecting class ------------------------------------------------------------ }

function DetectClassJPG(const Stream: TStream): TEncodedImageClass;
begin
  { JPG format cannot express grayscale or alpha. }
  Result := TRGBImage;
end;

function InternalDetectClassPNG(const Stream: TStream): TEncodedImageClass;

  function ChunkTypeCode(const ChunkName: String): UInt32;
  begin
    Assert(Length(ChunkName) = SizeOf(UInt32));
    Result :=
      {$ifdef CASTLE_LITTLE_ENDIAN}
      UInt32(Ord(ChunkName[4])) shl 24 +
      UInt32(Ord(ChunkName[3])) shl 16 +
      UInt32(Ord(ChunkName[2])) shl 8 +
      Ord(ChunkName[1])
      {$else}
      UInt32(Ord(ChunkName[1])) shl 24 +
      UInt32(Ord(ChunkName[2])) shl 16 +
      UInt32(Ord(ChunkName[3])) shl 8 +
      Ord(ChunkName[4])
      {$endif}
    ;
  end;

const
  { Color type masks and types.
    Copied from castleinternalpng_dynamic.inc, so this compiles regardless of libpng unit. }
  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR = 2;
  PNG_COLOR_MASK_ALPHA = 4;

  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE);
  PNG_COLOR_TYPE_RGB = (PNG_COLOR_MASK_COLOR);
  PNG_COLOR_TYPE_RGB_ALPHA = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA);
  PNG_COLOR_TYPE_GRAY_ALPHA = (PNG_COLOR_MASK_ALPHA);
var
  ReadHeader, ChunkType, ChunkSize, IWidth, IHeight: UInt32;
  ChunkTypeIHDR, ChunkTypetRNS, ChunkTypeIEND: UInt32;
  Bits, ColorType: Byte;
begin
  try
    { See https://en.wikipedia.org/wiki/Portable_Network_Graphics about PNG header }

    ChunkTypeIHDR := ChunkTypeCode('IHDR');
    ChunkTypetRNS := ChunkTypeCode('tRNS');
    ChunkTypeIEND := ChunkTypeCode('IEND');

    Stream.ReadBuffer(ReadHeader, SizeOf(ReadHeader));
    if BEtoN(ReadHeader) <> $89504E47 then
      Exit(nil);

    Stream.ReadBuffer(ReadHeader, SizeOf(ReadHeader));
    if BEtoN(ReadHeader) <> $0D0A1A0A then
      Exit(nil);

    Stream.ReadBuffer(ChunkSize, SizeOf(ChunkSize));
    if BEtoN(ChunkSize) <> 13 then
      Exit(nil);

    Stream.ReadBuffer(ChunkType, SizeOf(ChunkType));
    if LEtoN(ChunkType) <> ChunkTypeIHDR then // chunk IHDR must be first
      Exit(nil);

    Stream.ReadBuffer(IWidth, SizeOf(IWidth));
    // IWidth := BEtoN(IWidth); // not needed, as we don't actually use it
    Stream.ReadBuffer(IHeight, SizeOf(IHeight));
    // IHeight := BEtoN(IHeight); // not needed, as we don't actually use it
    Stream.ReadBuffer(Bits, SizeOf(Bits));
    Stream.ReadBuffer(ColorType, SizeOf(ColorType));

    case ColorType of
      PNG_COLOR_TYPE_GRAY      : Result := TGrayscaleImage;
      PNG_COLOR_TYPE_RGB       : Result := TRGBImage;
      PNG_COLOR_TYPE_RGB_ALPHA : Result := TRGBAlphaImage;
      PNG_COLOR_TYPE_GRAY_ALPHA: Result := TGrayscaleAlphaImage;
      else                       Result := nil; // unknown, possible in case of PNG_COLOR_TYPE_PALETTE
    end;

    // if Result indicates non-alpha image, scan for PNG tRNS chunk
    if (Result = TRGBImage) or
       (Result = TGrayscaleImage) then
    begin
      { skip 3 remaining bytes of IHDR chunk, + 4 for CRC }
      Stream.Position := Stream.Position + 3 + 4;

      { We need to detect whether we have tRNS chunk.
        Libpng implementation does this by

          png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0

        In this case, we have alpha information,
        even though ColorType doesn't indicate it.
        See png_is_alpha implementation in castleinternalpng.pas.
      }

      while ChunkType <> ChunkTypeIEND do
      begin
        Stream.ReadBuffer(ChunkSize, SizeOf(ChunkSize));
        ChunkSize := BEtoN(ChunkSize);

        Stream.ReadBuffer(ChunkType, SizeOf(ChunkType));

        // if tRNS, then update Result class and break
        if LEtoN(ChunkType) = ChunkTypetRNS then
        begin
          if Result = TRGBImage then
          begin
            Result := TRGBAlphaImage;
            Break;
          end else
          if Result = TGrayscaleImage then
          begin
            Result := TGrayscaleAlphaImage;
            Break;
          end;
        end;

        Stream.Position := Stream.Position + ChunkSize + 4 { CRC };
      end;
    end;
  finally
    Stream.Position := 0;
  end;
end;

{ File formats list ---------------------------------------------------------- }

type
  { See TImageFormatInfo.Load }
  TImageLoadFunc = function (const Stream: TStream;
    const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;

  { See TImageFormatInfo.Save }
  TImageSaveFunc = procedure (const Img: TEncodedImage; const Stream: TStream);

  { See TImageFormatInfo.DetectClass }
  TImageDetectClassFunc = function (const Stream: TStream): TEncodedImageClass;

  { Possible TEncodedImage classes supported by Save method of this file format. }
  TImageSaveHandledClasses = (
    scRGB,
    scG_GA_RGB_RGBA,
    scG_GA_RGB_RGBA_GPUCompressed,
    scRGB_RGBFloat
  );

  { Index of TImageFormatInfo.MimeTypes array and
    type for TImageFormatInfo.MimeTypesCount.
    Implies that TImageFormatInfo.MimeTypes is indexed from 1,
    TImageFormatInfo.MimeTypesCount must be >= 1,
    so each file format must have at least one
    (treated as "default" in some cases) MIME type. }
  TImageFormatInfoMimeTypesCount = 1..7;

  { A type to index TImageFormatInfo.Exts array and also for TImageFormatInfo.ExtsCount.
    So TImageFormatInfo.Exts array is indexed from 1,
    and TImageFormatInfo.ExtsCount must be >= 1, so each file format must have at least one
    (treated as "default" in some cases) file extension. }
  TImageFormatInfoExtsCount = 1..4;

  { Record with information specific to TImageFormat. }
  TImageFormatInfo = record
    { Human-readable format name.

      Note that this is supposed to be shown to normal user,
      in save dialog boxes etc. So it should be short and concise. I used to
      have here long format names like @code(JFIF, JPEG File Interchange Format) or
      @code(PNG, Portable Network Graphic), but they are too ugly, and unnecessarily
      resolving format abbrevs. For example, most users probably used JPEG,
      but not many have to know, or understand, that actually this is image format JFIF;
      these are technical and historical details that are not needed for normal usage of image
      operations.

      Saying it directly, I want to keep this FormatName short and concise.
      This is not a place to educate users what some abbrev means.
      This is a place to "name" each file format in the most natural way, which
      usually means to only slightly rephrase typical file format extension.

      In practice, I now copy descriptions from English GIMP open dialog. }
    FormatName: string;

    MimeTypesCount: TImageFormatInfoMimeTypesCount;

    { MIME types recognized as this image file format.
      First MIME type is the default for this file format
      (some procedures make use of it). }
    MimeTypes: array [TImageFormatInfoMimeTypesCount] of string;

    ExtsCount: TImageFormatInfoExtsCount;

    { File extensions for this image type.
      First file extension is default, which is used for some routines.
      Must be lowercase.

      This is used e.g. to construct file filters in open/save dialogs.
      Together with MimeTypes it is also used by URIMimeType to map
      file extension into a MIME type. An extension matching one of Exts
      values implicates the default MIME type for this format (MimeTypes[1]).

      Note that to cooperate nicely with network URLs
      (when server may report MIME type) and data URIs, most of the code
      should operate using MIME types instead of file extensions.
      So usually you are more interested in MimeTypes than Exts. }
    Exts: array [TImageFormatInfoExtsCount] of string;

    { Load method for this file format.
      @nil if cannot be loaded. }
    Load: TImageLoadFunc;

    { Save method for this file format.
      @nil if cannot be saved. }
    Save: TImageSaveFunc;

    { If Save is assigned, this describes what TEncodedImage descendants
      can be handled by this Save. SaveImage needs this information,
      to make necessary conversions from other TEncodedImage classes,
      when possible. }
    SavedClasses: TImageSaveHandledClasses;

    { Detect the class to which load image, based on Stream.
      Return @nil if auto-detection failed.

      If this function actually reads some Stream contents,
      it @italic(must) reset Stream.Position to 0 afterwards
      (we don't do it automatically, to avoid exceptions in case this function
      doesn't read from Stream, and stream doesn't support "Stream.Position := 0").
    }
    DetectClass: TImageDetectClassFunc;
  end;

const
  { Information about supported image formats. }
  ImageFormatInfos: array [TImageFormat] of TImageFormatInfo =
  (

    { Portable Network Graphic } { }
    ( FormatName: 'PNG image';
      MimeTypesCount: 1;
      MimeTypes: ('image/png', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('png', '', '', '');
      { Regardless of using libpng or not, our PNG load/save can handle
        all possibilities. }
      Load: {$ifdef FPC}@{$endif}LoadPNG;
      Save: {$ifdef FPC}@{$endif}SavePNG; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: {$ifdef FPC}@{$endif}InternalDetectClassPNG;
    ),
    ( FormatName: 'Windows BMP image';
      MimeTypesCount: 1;
      MimeTypes: ('image/bmp', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('bmp', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadBMP;
      Save: {$ifdef FPC}@{$endif}SaveBMP; SavedClasses: scRGB;
      DetectClass: nil;
    ),
    { JFIF, JPEG File Interchange Format } { }
    ( FormatName: 'JPEG image';
      MimeTypesCount: 2;
      MimeTypes: ('image/jpeg', 'image/jpg', '', '', '', '', '');
      ExtsCount: 3; Exts: ('jpg', 'jpeg', 'jpe', '');
      Load: {$ifdef FPC}@{$endif}LoadJPEG;
      Save: {$ifdef FPC}@{$endif}SaveJPEG; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: {$ifdef FPC}@{$endif}DetectClassJPG;
    ),
    { Graphics Interchange Format } { }
    ( FormatName: 'GIF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/gif', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('gif', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadGIF;
      Save: {$ifdef FPC}@{$endif}SaveGIF; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    ),
    ( FormatName: 'TarGA image';
      MimeTypesCount: 2;
      MimeTypes: ('image/x-targa', 'image/x-tga', '', '', '', '', '');
      ExtsCount: 2; Exts: ('tga', 'tpic', '', '');
      Load: {$ifdef FPC}@{$endif}LoadTGA;
      Save: {$ifdef FPC}@{$endif}SaveTGA; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    ),
    ( FormatName: 'XPM image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-xpixmap', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('xpm', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadXPM;
      Save: {$ifdef FPC}@{$endif}SaveXPM; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    ),
    ( FormatName: 'PSD image';
      MimeTypesCount: 4;
      MimeTypes: ('image/photoshop', 'image/x-photoshop', 'image/psd', 'application/photoshop', '', '', '');
      ExtsCount: 1; Exts: ('psd', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadPSD;
      Save: nil; SavedClasses: scRGB;
      DetectClass: nil;
    ),
    ( FormatName: 'ZSoft PCX image';
      MimeTypesCount: 5;
      MimeTypes: ('image/pcx', 'application/pcx', 'application/x-pcx', 'image/x-pc-paintbrush', 'image/x-pcx', '', '');
      ExtsCount: 1; Exts: ('pcx', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadPCX;
      Save: nil; SavedClasses: scRGB;
      DetectClass: nil;
    ),
    ( FormatName: 'PNM image';
      MimeTypesCount: 7;
      MimeTypes: ('image/x-portable-anymap', 'image/x-portable-graymap', 'image/x-pgm', 'image/x-portable-bitmap', 'image/pbm', 'image/x-pbm', 'image/x-portable-pixmap');
      ExtsCount: 4; Exts: ('pnm', 'pgm', 'pbm', 'ppm');
      Load: {$ifdef FPC}@{$endif}LoadPNM;
      Save: {$ifdef FPC}@{$endif}SavePNM; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    ),

    { Direct Draw Surface } { }
    ( FormatName: 'DDS image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-dds', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('dds', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadDDS;
      Save: {$ifdef FPC}@{$endif}SaveDDS; SavedClasses: scG_GA_RGB_RGBA_GPUCompressed;
      DetectClass: nil;
    ),

    { Khronos KTX } { }
    ( FormatName: 'Khronos KTX image';
      MimeTypesCount: 1;
      MimeTypes: ('image/ktx', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ktx', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadKTX;
      Save: nil; SavedClasses: scG_GA_RGB_RGBA_GPUCompressed;
      DetectClass: nil;
    ),
    { ASTC } { }
    ( FormatName: 'ASTC image';
      MimeTypesCount: 1;
      MimeTypes: ('image/astc', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('astc', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadASTC;
      Save: nil; SavedClasses: scG_GA_RGB_RGBA_GPUCompressed;
      DetectClass: nil;
    ),
    ( FormatName: 'RGBE (Radiance, HDR) image';
      MimeTypesCount: 1;
      MimeTypes: ('image/vnd.radiance', '', '', '', '', '', '');
      ExtsCount: 3; Exts: ('rgbe', 'pic', 'hdr', '');
      Load: {$ifdef FPC}@{$endif}LoadRGBE;
      Save: {$ifdef FPC}@{$endif}SaveRGBE; SavedClasses: scRGB_RGBFloat;
      DetectClass: nil;
    ),
    ( FormatName: 'IPLab image';
      MimeTypesCount: 1;
      { IPL MIME type invented by Michalis, to make it unique to communicate image format for LoadImage } { }
      MimeTypes: ('image/x-ipl', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ipl', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadIPL;
      Save: nil; SavedClasses: scRGB;
      DetectClass: nil;
    )

    {$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
    ,
    ( FormatName: 'TIFF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/tiff', '', '', '', '', '', '');
      ExtsCount: 2; Exts: ('tiff', 'tif', '', '');
      Load: {$ifdef FPC}@{$endif}LoadTIFF;
      Save: {$ifdef FPC}@{$endif}SaveTIFF; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    ),
    ( FormatName: 'JPEG 2000 image';
      MimeTypesCount: 4;
      MimeTypes: ('image/jp2', 'image/jpeg2000', 'image/jpeg2000-image', 'image/x-jpeg2000-image', '', '', '');
      ExtsCount: 1; Exts: ('jp2', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadJP2;
      Save: {$ifdef FPC}@{$endif}SaveJP2; SavedClasses: scG_GA_RGB_RGBA;
      DetectClass: nil;
    )
    {$endif CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}

    (*
    ( FormatName: 'EXR image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-exr', '', '', '', '', '', '');
      ExtsCount: 1; Exts: ('exr', '', '', '');
      Load: {$ifdef FPC}@{$endif}LoadEXR;
      Save: nil; SavedClasses: scRGB;
      DetectClass: nil;
    )
    *)
  );

{ image loading utilities --------------------------------------------------- }

{ Helper methods for implementing LoadEncodedImage. }

function ClassAllowed(ImageClass: TEncodedImageClass;
  const AllowedImageClasses: array of TEncodedImageClass): boolean;
begin
  Result := (High(AllowedImageClasses) = -1) or
    InImageClasses(ImageClass, AllowedImageClasses);
end;

function LoadEncodedImageParams(
  const AllowedImageClasses: array of TEncodedImageClass): string;

  function ImageClassesToStr(const AllowedImageClasses: array of TEncodedImageClass): string;
  var
    I: Integer;
  begin
    if High(AllowedImageClasses) = -1 then
      Result := 'all' else
    begin
      Result := '';
      for I := 0 to High(AllowedImageClasses) do
      begin
        if Result <> '' then Result := Result + ', ';
        Result := Result + AllowedImageClasses[I].ClassName;
      end;
    end;
  end;

begin
  Result := 'required class [' + ImageClassesToStr(AllowedImageClasses) + ']';
end;

{ Exposing ImageFormatInfos mimo/exts info ----------------------------------- }

function ImageExtToMimeType(Ext: string): string;
var
  I: TImageFormat;
  E: TImageFormatInfoExtsCount;
begin
  Ext := LowerCase(Ext);
  for I := Low(I) to High(I) do
    for E := Low(E) to ImageFormatInfos[I].ExtsCount do
      if Ext = '.' + ImageFormatInfos[I].Exts[E] then
        Exit(ImageFormatInfos[I].MimeTypes[1]);
  Result := '';
end;

procedure RegisterMimeTypes;
var
  I: TImageFormat;
  E: TImageFormatInfoExtsCount;
begin
  for I := Low(I) to High(I) do
    for E := Low(E) to ImageFormatInfos[I].ExtsCount do
      URIMimeExtensions.AddOrSetValue(
        '.' + ImageFormatInfos[I].Exts[E],
        ImageFormatInfos[I].MimeTypes[1]);
end;

function MimeTypeToImageFormat(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;
var
  I: TImageFormat;
  M: TImageFormatInfoMimeTypesCount;
begin
  for I := Low(I) to High(I) do
  begin
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[I].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[I].Save)) then
    for M := 1 to ImageFormatInfos[I].MimeTypesCount do
      if MimeType = ImageFormatInfos[I].MimeTypes[M] then
      begin
        ImgFormat := I;
        Exit(true);
      end;
  end;
  Result := false;
end;

function ImageClassBestForSavingToFormatCore(const Format: TImageFormat): TCastleImageClass;
begin
  if Format = ifRGBE then
    Result := TRGBFloatImage else
    Result := TRGBImage;
end;

function ImageClassBestForSavingToFormat(const URL: string): TCastleImageClass;
var
  Format: TImageFormat;
begin
  if not MimeTypeToImageFormat(URIMimeType(URL), false, true, Format) then
    Exit(TRGBImage);
  Result := ImageClassBestForSavingToFormatCore(Format);
end;

function IsImageMimeType(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean): boolean;
var
  ImgFormat: TImageFormat;
begin
  Result := MimeTypeToImageFormat(MimeType, OnlyLoadable, OnlySaveable, ImgFormat);
end;

function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean;
  const LinePrefix: string): string;
var
  Format: TImageFormat;
  I: integer;
begin
  result := '';

  for Format := Low(Format) to High(Format) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[Format].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[Format].Save)) then
    begin
      { Note that we do not want to add new line at the end because there
        it would not be very convenient to recognize whether we are at the last
        line or not (due to OnlySaveable / OnlyLoadable it cannot be recognized
        by simply checking Format <High (Format)). }
      if Result <> '' then
        Result := Result + NL;

      Result := Result + LinePrefix + ImageFormatInfos[Format].Exts[1];
      for I := 2 to ImageFormatInfos[Format].ExtsCount do
        Result := Result + ', ' + ImageFormatInfos[Format].Exts[I];
      Result := Result + ' - ' + ImageFormatInfos[Format].FormatName;
    end;
end;

function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
var
  Format: TImageFormat;
  I: integer;
begin
  Result := '';

  for Format := Low(Format) to High(Format) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[Format].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[Format].Save)) then
    begin
      for I := 1 to ImageFormatInfos[Format].ExtsCount do
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + ImageFormatInfos[Format].Exts[I];
      end;
    end;
end;

procedure InitializeImagesFileFilters;

  function CreateImagesFilters: TFileFilterList;
  begin
    Result := TFileFilterList.Create(true);
    Result.AddFilter('All Files', ['*']);
    Result.AddFilter('All Images', []);
    Result.DefaultFilter := 1;
  end;

  procedure AddImageFormat(Filters: TFileFilterList; Format: TImageFormatInfo);
  var
    F: TFileFilter;
    ExtIndex: Integer;
    Pattern: string;
  begin
    F := TFileFilter.Create;
    Filters.Add(F);
    F.Name := Format.FormatName + ' (';

    for ExtIndex := 1 to Format.ExtsCount do
    begin
      Pattern := '*.' + Format.Exts[ExtIndex];

      { add to "All images" filter }
      Filters[Filters.DefaultFilter].Patterns.Append(Pattern);

      { add to this filter }
      F.Patterns.Append(Pattern);

      { add to this filter visible name }
      if ExtIndex <> 1 then F.Name := F.Name + ', ';
      F.Name := F.Name + Pattern;
    end;

    F.Name := F.Name + ')';
  end;

var
  Format: TImageFormat;
begin
  LoadImage_FileFilters := CreateImagesFilters;
  SaveImage_FileFilters := CreateImagesFilters;

  for Format := Low(Format) to High(Format) do
  begin
    if Assigned(ImageFormatInfos[Format].Load) then
      AddImageFormat(LoadImage_FileFilters, ImageFormatInfos[Format]);
    if Assigned(ImageFormatInfos[Format].Save) then
      AddImageFormat(SaveImage_FileFilters, ImageFormatInfos[Format]);
  end;
end;
