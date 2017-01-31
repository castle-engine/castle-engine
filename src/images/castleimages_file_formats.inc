{
  Copyright 2001-2017 Michalis Kamburelis.

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
  { }
  TImageFormat = (
    { We handle PNG file format fully, both reading and writing,
      through the libpng library.

      This format supports a full alpha channel.
      Besides PSD, this is the only format that allows full-range
      (partial transparency) alpha channel.

      Trying to read / write PNG file when libpng is not installed
      (through LoadImage, LoadEncodedImage, SaveImage, LoadPNG, SavePNG and others)
      will raise exception ELibPngNotAvailable. Note that the check
      for availability of libpng is done only once you try to load/save PNG file.
      You can perfectly compile and even run your programs without
      PNG installed, until you try to load/save PNG format. }
    ifPNG,

    { We handle uncompressed BMP images. }
    ifBMP,

    ifPPM,

    { Image formats below are supported by FPImage. }
    ifJPEG, ifGIF, ifTGA, ifXPM, ifPSD, ifPCX, ifPNM,

    { We handle fully DDS (DirectDraw Surface) image format.
      See also TCompositeImage class in CastleCompositeImage unit,
      that exposes even more features of the DDS image format. }
    ifDDS,

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

    ifIPL,

    { Image formats below are supported
      by converting them  "under the hood" with ImageMagick.
      This is available only if this unit is compiled with FPC
      (i.e. not with Delphi) on platforms where ExecuteProcess is
      implemented. And ImageMagick must be installed and available on $PATH. }
    ifTIFF, ifSGI, ifJP2, ifEXR
  );
  TImageFormats = set of TImageFormat;

{ Loading image (format-specific) ---------------------------------------

  Load image from Stream.

  They must honour AllowedImageClasses, just like
  LoadImage and LoadEncodedImage do. Except they don't have to care about returning all TEncodedImage
  descendants: see @link(TImageFormatInfo.LoadedClasses). So higher-level
  LoadImage and LoadEncodedImage will use them and eventually convert their result.

  An appropriate descendant of EImageLoadError will be raised
  in case of error when reading from Stream or when Stream will not
  contain correct data. }

type
  EInvalidBMP = class(EInvalidImageFormat);
  EInvalidPNG = class(EInvalidImageFormat);
  EInvalidPPM = class(EInvalidImageFormat);
  EInvalidIPL = class(EInvalidImageFormat);
  EInvalidRGBE = class(EInvalidImageFormat);

function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadBMP(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadSGI(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadJP2(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadEXR(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load PPM image.
  Loads only the first image in .ppm file. }
function LoadPPM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load PNM image (PNM, PGM, PBM, PPM) through FpImage.
  Note that for PPM, for now it's more advised to use our LoadPPM. }
function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

function LoadIPL(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load RGBE image.
  This low-level function can load to TRGBFloatImage (preserving image data)
  or to TRGBImage (loosing floating point precision of RGBE format). }
function LoadRGBE(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage; forward;

{ Load DDS image file into a single 2D image. This simply returns the first
  image found in DDS file, which should be the main image.
  If you want to investigate other images in DDS, you have to use TCompositeImage
  class. }
function LoadDDS(Stream: TStream;
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
procedure SaveBMP(Img: TEncodedImage; Stream: TStream); forward;
procedure SavePNG(Img: TEncodedImage; Stream: TStream); forward;
{ }
procedure SaveJPEG(Img: TEncodedImage; Stream: TStream); forward;
{ }
procedure SavePPM(Img: TEncodedImage; Stream: TStream; binary: boolean); overload; forward;
procedure SavePPM(Img: TEncodedImage; Stream: TStream); { binary = true } overload; forward;
{ }
procedure SaveRGBE(Img: TEncodedImage; Stream: TStream); forward;

procedure SaveDDS(Img: TEncodedImage; Stream: TStream); forward;

{ File formats list ---------------------------------------------------------- }

type
  TImageLoadFunc = function (Stream: TStream;
    const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
  TImageSaveFunc = procedure (Img: TEncodedImage; Stream: TStream);

  { Possible TEncodedImage classes that can be returned by Load method
    of this file format. It's assumed that appropriate Load can return
    only these classes, and any of these classes,
    and can convert between them.

    If the LoadImage or LoadEncodedImage will be called allowing some TEncodedImage descendants
    that can be returned by Load of this format,
    then LoadImage or LoadEncodedImage will pretty much just pass the call to Load
    for appropriate file format.
    The above is expected to be the most common and most efficient case.
    This way necessary conversion (e.g. adding alpha channel) can be
    done at the lowest level, right inside image format handler,
    which means that e.g. you can do it per-pixel, or by libpng transforms
    in case of PNG format.

    Only when it's not possible (if, and only if, none of the AllowedImageClasses
    specified in LoadImage or LoadEncodedImage call can be returned by Load of this format)
    then LoadImage and LoadEncodedImage will try more elaborate approach. This means that
    it will try using Load of this image format, followed by
    some conversions of the image afterwards. This is generally less
    efficient, as it means that temporary image will be created during
    loading.
  }
  TImageLoadHandledClasses = (
    lcRGB,
    lcRGB_RGBA,
    lcG_GA_RGB_RGBA,
    lcG_GA_RGB_RGBA_GPUCompressed,
    lcRGB_RGBFloat
  );

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
  TImageFormatInfoMimeTypesCount = 1..6;

  { A type to index TImageFormatInfo.Exts array and also for TImageFormatInfo.ExtsCount.
    So TImageFormatInfo.Exts array is indexed from 1,
    and TImageFormatInfo.ExtsCount must be >= 1, so each file format must have at least one
    (treated as "default" in some cases) file extension. }
  TImageFormatInfoExtsCount = 1..3;

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

    { If Load is assigned, this describes what TEncodedImage descendants
      can be returned by this Load. LoadImage and LoadEncodedImage need this information,
      to make necessary conversions to other TEncodedImage classes,
      when possible. }
    LoadedClasses: TImageLoadHandledClasses;

    { Save method for this file format.
      @nil if cannot be saved. }
    Save: TImageSaveFunc;
    SavedClasses: TImageSaveHandledClasses;
  end;

const
  { Information about supported image formats. }
  ImageFormatInfos: array [TImageFormat] of TImageFormatInfo =
  ( { The order on this list matters --- it determines the order of filters
      for open/save dialogs.
      First list most adviced and well-known formats, starting from lossless. }

    { Portable Network Graphic } { }
    ( FormatName: 'PNG image';
      MimeTypesCount: 1;
      MimeTypes: ('image/png', '', '', '', '', '');
      ExtsCount: 1; Exts: ('png', '', '');
      { Regardless of CASTLE_PNG_USING_FCL_IMAGE, png load/save can handle
        all possibilities. }
      Load: @LoadPNG; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: @SavePNG; SavedClasses: scG_GA_RGB_RGBA; ),
    ( FormatName: 'Windows BMP image';
      MimeTypesCount: 1;
      MimeTypes: ('image/bmp', '', '', '', '', '');
      ExtsCount: 1; Exts: ('bmp', '', '');
      Load: @LoadBMP; LoadedClasses: lcRGB_RGBA;
      Save: @SaveBMP; SavedClasses: scRGB),
    { Portable Pixel Map } { }
    ( FormatName: 'PPM image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-portable-pixmap', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ppm', '', '');
      Load: @LoadPPM; LoadedClasses: lcRGB;
      Save: @SavePPM; SavedClasses: scRGB; ),
    { JFIF, JPEG File Interchange Format } { }
    ( FormatName: 'JPEG image';
      MimeTypesCount: 2;
      MimeTypes: ('image/jpeg', 'image/jpg', '', '', '', '');
      ExtsCount: 3; Exts: ('jpg', 'jpeg', 'jpe');
      Load: @LoadJPEG; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: @SaveJPEG; SavedClasses: scG_GA_RGB_RGBA; ),
    { Graphics Interchange Format } { }
    ( FormatName: 'GIF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/gif', '', '', '', '', '');
      ExtsCount: 1; Exts: ('gif', '', '');
      Load: @LoadGIF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'TarGA image';
      MimeTypesCount: 2;
      MimeTypes: ('image/x-targa', 'image/x-tga', '', '', '', '');
      ExtsCount: 2; Exts: ('tga', 'tpic', '');
      Load: @LoadTGA; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'XPM image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-xpixmap', '', '', '', '', '');
      ExtsCount: 1; Exts: ('xpm', '', '');
      Load: @LoadXPM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PSD image';
      MimeTypesCount: 4;
      MimeTypes: ('image/photoshop', 'image/x-photoshop', 'image/psd', 'application/photoshop', '', '');
      ExtsCount: 1; Exts: ('psd', '', '');
      Load: @LoadPSD; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'ZSoft PCX image';
      MimeTypesCount: 5;
      MimeTypes: ('image/pcx', 'application/pcx', 'application/x-pcx', 'image/x-pc-paintbrush', 'image/x-pcx', '');
      ExtsCount: 1; Exts: ('pcx', '', '');
      Load: @LoadPCX; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PNM image';
      MimeTypesCount: 6;
      MimeTypes: ('image/x-portable-anymap', 'image/x-portable-graymap', 'image/x-pgm', 'image/x-portable-bitmap', 'image/pbm', 'image/x-pbm');
      ExtsCount: 3; Exts: ('pnm', 'pgm', 'pbm');
      Load: @LoadPNM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),

    { Direct Draw Surface } { }
    ( FormatName: 'DDS image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-dds', '', '', '', '', '');
      ExtsCount: 1; Exts: ('dds', '', '');
      Load: @LoadDDS; LoadedClasses: lcG_GA_RGB_RGBA_GPUCompressed;
      Save: @SaveDDS; SavedClasses: scG_GA_RGB_RGBA_GPUCompressed; ),

    { Image formats not well known. }

    ( FormatName: 'RGBE (RGB+Exponent) image';
      MimeTypesCount: 1;
      MimeTypes: ('image/vnd.radiance', '', '', '', '', '');
      ExtsCount: 3; Exts: ('rgbe', 'pic', 'hdr');
      Load: @LoadRGBE; LoadedClasses: lcRGB_RGBFloat;
      Save: @SaveRGBE; SavedClasses: scRGB_RGBFloat; ),
    ( FormatName: 'IPLab image';
      MimeTypesCount: 1;
      { ipl MIME type invented by Kambi, to make it unique to communicate image format for LoadImage } { }
      MimeTypes: ('image/x-ipl', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ipl', '', '');
      Load: @LoadIPL; LoadedClasses: lcRGB;
      Save: nil; SavedClasses: scRGB; ),

    { Image formats loaded using ImageMagick's convert.
      Placed at the end of the list, to be at the end of open/save dialogs
      filters, since there's a large chance they will not work,
      if user didn't install ImageMagick. } { }

    ( FormatName: 'TIFF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/tiff', '', '', '', '', '');
      ExtsCount: 2; Exts: ('tiff', 'tif', '');
      Load: @LoadTIFF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'SGI image';
      MimeTypesCount: 3;
      MimeTypes: ('image/sgi', 'image/x-sgi', 'image/x-sgi-rgba', '', '', '');
      ExtsCount: 1; Exts: ('sgi', '', '');
      Load: @LoadSGI; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'JPEG 2000 image';
      MimeTypesCount: 4;
      MimeTypes: ('image/jp2', 'image/jpeg2000', 'image/jpeg2000-image', 'image/x-jpeg2000-image', '', '');
      ExtsCount: 1; Exts: ('jp2', '', '');
      Load: @LoadJP2; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'EXR image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-exr', '', '', '', '', '');
      ExtsCount: 1; Exts: ('exr', '', '');
      Load: @LoadEXR; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; )
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
        if Result <> '' then Result += ', ';
        Result += AllowedImageClasses[I].ClassName;
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

function ImageClassBestForSavingToFormat(const Format: TImageFormat): TCastleImageClass;
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
  Result := ImageClassBestForSavingToFormat(Format);
end;

function IsImageMimeType(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean): boolean;
var
  ImgFormat: TImageFormat;
begin
  Result := MimeTypeToImageFormat(MimeType, OnlyLoadable, OnlySaveable, ImgFormat);
end;

function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
var
  iff: TImageFormat;
  i: integer;
begin
  result := '';

  for iff := Low(iff) to High(iff) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].Save)) then
    begin
      { zwrocmy uwage ze nie chcemy doklejac nl na koncu (bo zalatwieniu
        sprawy z formatem iff) bo tam nie byloby zbyt wygodnie rozpoznawac
        czy jestesmy ostatnia linia czy nie (na skutek OnlySaveable/OnlyLoadable
        nie mozna tego rozpoznac prostym sprawdzeniem iff < High(iff) }
      if result <> '' then result := result + nl;

      result := result +LinePrefix +ImageFormatInfos[iff].exts[1];
      for i := 2 to ImageFormatInfos[iff].extsCount do
        result := result + ', ' +ImageFormatInfos[iff].exts[i];
      result := result + ' - '+ImageFormatInfos[iff].formatName;
    end;
end;

function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
var
  iff: TImageFormat;
  i: integer;
begin
  result := '';

  for iff := Low(iff) to High(iff) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].Save)) then
    begin
      for i := 1 to ImageFormatInfos[iff].extsCount do
      begin
        if result <> '' then result := result + ', ';
        result := result + ImageFormatInfos[iff].exts[i];
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
