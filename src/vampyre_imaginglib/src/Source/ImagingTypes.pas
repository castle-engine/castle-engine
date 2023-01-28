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

{ This unit contains basic types and constants used by Imaging library.}
unit ImagingTypes;

{$I ImagingOptions.inc}

interface

const
  { Current Major version of Imaging.}
  ImagingVersionMajor = 0;
  { Current Minor version of Imaging.}
  ImagingVersionMinor = 82;

  { Imaging Option Ids whose values can be set/get by SetOption/
    GetOption functions.}

  { Defines Jpeg compression quality, ranges from 1 (ugly/small) to 100 (nice/large).
    Default value is 90.}
  ImagingJpegQuality           = 10;
  { Specifies whether Jpeg images are saved in progressive format,
    can be 0 or 1. Default value is 0.}
  ImagingJpegProgressive       = 11;

  { Specifies whether Windows Bitmaps are saved using RLE compression
    (only for 1/4/8 bit images), can be 0 or 1. Default value is 1.}
  ImagingBitmapRLE             = 12;

  { Specifies whether Targa images are saved using RLE compression,
    can be 0 or 1. Default value is 0.}
  ImagingTargaRLE              = 13;

  { Value of this option is non-zero if last loaded DDS file was cube map.}
  ImagingDDSLoadedCubeMap      = 14;
  { Value of this option is non-zero if last loaded DDS file was volume texture.}
  ImagingDDSLoadedVolume       = 15;
  { Value of this option is number of mipmap levels of last loaded DDS image.}
  ImagingDDSLoadedMipMapCount  = 16;
  { Value of this option is depth (slices of volume texture or faces of
    cube map) of last loaded DDS image.}
  ImagingDDSLoadedDepth        = 17;
  { If it is non-zero next saved DDS file should be stored as cube map.}
  ImagingDDSSaveCubeMap        = 18;
  { If it is non-zero next saved DDS file should be stored as volume texture.}
  ImagingDDSSaveVolume         = 19;
  { Sets the number of mipmaps which should be stored in the next saved DDS file.
    Only applies to cube maps and volumes, ordinary 2D textures save all
    levels present in input.}
  ImagingDDSSaveMipMapCount    = 20;
  { Sets the depth (slices of volume texture or faces of cube map)
    of the next saved DDS file.}
  ImagingDDSSaveDepth          = 21;

  { Sets precompression filter used when saving PNG images. Allowed values
    are: 0 (none), 1 (sub), 2 (up), 3 (average), 4 (paeth),
    5 (use 0 for indexed/gray images and 4 for RGB/ARGB images),
    6 (adaptive filtering - use best filter for each scanline - very slow).
    Note that filters 3 and 4 are much slower than filters 1 and 2.
    Default value is 5.}
  ImagingPNGPreFilter          = 25;
  { Sets ZLib compression level used when saving PNG images.
    Allowed values are in range 0 (no compression) to 9 (best compression).
    Default value is 5.}
  ImagingPNGCompressLevel      = 26;
  { Boolean option that specifies whether PNG images with more frames (APNG format)
    are animated by Imaging (according to frame disposal/blend methods) or just
    raw frames are loaded and sent to user (if you want to animate APNG yourself).
    Default value is 1.}
  ImagingPNGLoadAnimated       = 27;
  { Sets ZLib compression strategy used when saving PNG files (see deflateInit2()
    in ZLib for details). Allowed values are: 0 (default), 1 (filtered),
    2 (huffman only). Default value is 0.}
  ImagingPNGZLibStrategy       = 28;

  { Specifies whether MNG animation frames are saved with lossy or lossless
    compression. Lossless frames are saved as PNG images and lossy frames are
    saved as JNG images. Allowed values are 0 (False) and 1 (True).
    Default value is 0.}
  ImagingMNGLossyCompression   = 32;
  { Defines whether alpha channel of lossy compressed MNG frames
    (when ImagingMNGLossyCompression is 1) is lossy compressed too.
    Allowed values are 0 (False) and 1 (True). Default value is 0.}
  ImagingMNGLossyAlpha         = 33;
  { Sets precompression filter used when saving MNG frames as PNG images.
    For details look at ImagingPNGPreFilter.}
  ImagingMNGPreFilter          = 34;
  { Sets ZLib compression level used when saving MNG frames as PNG images.
    For details look at ImagingPNGCompressLevel.}
  ImagingMNGCompressLevel      = 35;
  { Specifies compression quality used when saving MNG frames as JNG images.
    For details look at ImagingJpegQuality.}
  ImagingMNGQuality            = 36;
  { Specifies whether images are saved in progressive format when saving MNG
    frames as JNG images. For details look at ImagingJpegProgressive.}
  ImagingMNGProgressive        = 37;

  { Specifies whether alpha channels of JNG images are lossy compressed.
    Allowed values are 0 (False) and 1 (True). Default value is 0.}
  ImagingJNGLossyAlpha         = 40;
  { Sets precompression filter used when saving lossless alpha channels.
    For details look at ImagingPNGPreFilter.}
  ImagingJNGAlphaPreFilter     = 41;
  { Sets ZLib compression level used when saving lossless alpha channels.
    For details look at ImagingPNGCompressLevel.}
  ImagingJNGAlphaCompressLevel = 42;
  { Defines compression quality used when saving JNG images (and lossy alpha channels).
    For details look at ImagingJpegQuality.}
  ImagingJNGQuality            = 43;
  { Specifies whether JNG images are saved in progressive format.
    For details look at ImagingJpegProgressive.}
  ImagingJNGProgressive        = 44;

  { Specifies whether PGM files are stored in text or in binary format.
    Allowed values are 0 (store as text - very! large files) and 1 (save binary).
    Default value is 1.}
  ImagingPGMSaveBinary         = 50;

  { Specifies whether PPM files are stored in text or in binary format.
    Allowed values are 0 (store as text - very! large files) and 1 (save binary).
    Default value is 1.}
  ImagingPPMSaveBinary         = 51;

  { Boolean option that specifies whether GIF images with more frames
    are animated by Imaging (according to frame disposal methods) or just
    raw frames are loaded and sent to user (if you want to animate GIF yourself).
    Default value is 1.
    Raw frames are 256 color indexed images (ifIndex8), whereas
    animated frames are always in 32bit ifA8R8G8B8 format (simplifies animating).}
  ImagingGIFLoadAnimated       = 56;

  { This option is used when reducing number of colors used in
    image (mainly when converting from ARGB image to indexed
    format). Mask is 'anded' (bitwise AND) with every pixel's
    channel value when creating color histogram. If $FF is used
    all 8bits of color channels are used which can result in very
    slow processing of large images with many colors so you can
    use lower masks to speed it up (FC, F8 and F0 are good
    choices). Allowed values are in range <0, $FF> and default is
    $FE.                                                          }
  ImagingColorReductionMask   = 128;
  { This option can be used to override image data format during image
    loading. If set to format different from ifUnknown all loaded images
    are automatically converted to this format. Useful when you have
    many files in various formats but you want them all in one format for
    further processing. Allowed values are in
    range <Ord(Low(TImageFormat)), Ord(High(TImageFormat))> and
    default value is ifUnknown.}
  ImagingLoadOverrideFormat   = 129;
  { This option can be used to override image data format during image
    saving. If set to format different from ifUnknown all images
    to be saved are automatically internally converted to this format.
    Note that image file formats support only a subset of Imaging data formats
    so final saved file may in different format than this override.
    Allowed values are in range <Ord(Low(TImageFormat)), Ord(High(TImageFormat))>
    and default value is ifUnknown.}
  ImagingSaveOverrideFormat   = 130;
  { Specifies resampling filter used when generating mipmaps. It is used
    in GenerateMipMaps low level function and Direct3D and OpenGL extensions.
    Allowed values are in range
    <Ord(Low(ImagingFormats.TSamplingFilter)), Ord(High(ImagingFormats.TSamplingFilter))>
    and default value is 1 (linear filter).}
  ImagingMipMapFilter         = 131;
  { Specifies threshold value used when automatically converting images to
    ifBinary format. For adaptive thresholding see ImagingBinary.pas unit.
    Default value is 128 and allowed range is 0..255.}
  ImagingBinaryThreshold       = 132;

  { Returned by GetOption if given Option Id is invalid.}
  InvalidOption = -$7FFFFFFF;

  { Indices that can be used to access channel values in array parts
    of structures like TColor32Rec. Note that this order can be
    used only for ARGB images. For ABGR image you must swap Red and Blue.}
  ChannelBlue  = 0;
  ChannelGreen = 1;
  ChannelRed   = 2;
  ChannelAlpha = 3;

type
{$IFDEF DCC}
  {$IF CompilerVersion <= 18.5}
    PtrUInt = Cardinal;
    PtrInt = Integer;
    { Some new Delphi platforms have 64bit LongInt/LongWord so rather use
      Int32/UInt32 where you really want 32bits. }
    Int32 = Integer;
    UInt32 = Cardinal;
    Int16 = SmallInt;
  {$ELSE}
    PtrUInt = NativeUInt;
    PtrInt = NativeInt;
  {$IFEND}
  { Not sure which Delphi version defined these (e.g. XE3 has UInt32 but not PUInt32). }
  {$IF not Defined(PInt32) or not Defined(PUInt32)}
    PInt32 = ^Int32;
    PUInt32 = ^UInt32;
  {$IFEND}
{$ENDIF}

  { Enum defining image data format. In formats with more channels,
    first channel after "if" is stored in the most significant bits and channel
    before end is stored in the least significant.}
  TImageFormat = (
    ifUnknown        = 0,
    ifDefault        = 1,
    { Indexed formats using palette }
    ifIndex8         = 10,
    { Grayscale/Luminance formats }
    ifGray8          = 40,
    ifA8Gray8        = 41,
    ifGray16         = 42,
    ifGray32         = 43,
    ifGray64         = 44,
    ifA16Gray16      = 45,
    { ARGB formats }
    ifX5R1G1B1       = 80,
    ifR3G3B2         = 81,
    ifR5G6B5         = 82,
    ifA1R5G5B5       = 83,
    ifA4R4G4B4       = 84,
    ifX1R5G5B5       = 85,
    ifX4R4G4B4       = 86,
    ifR8G8B8         = 87,
    ifA8R8G8B8       = 88,
    ifX8R8G8B8       = 89,
    ifR16G16B16      = 90,
    ifA16R16G16B16   = 91,
    ifB16G16R16      = 92,
    ifA16B16G16R16   = 93,
    { Floating point formats }
    ifR32F           = 160,
    ifA32R32G32B32F  = 161,
    ifA32B32G32R32F  = 162,
    ifR16F           = 163,
    ifA16R16G16B16F  = 164,
    ifA16B16G16R16F  = 165,
    ifR32G32B32F     = 166,
    ifB32G32R32F     = 167,
    { Special formats }
    ifDXT1           = 200,
    ifDXT3           = 201,
    ifDXT5           = 202,
    ifBTC            = 203,
    ifATI1N          = 204,
    ifATI2N          = 205,
    ifBinary         = 206,
    { Passthrough formats }
    {ifETC1           = 220,
    ifETC2RGB        = 221,
    ifETC2RGBA       = 222,
    ifETC2PA         = 223,
    ifDXBC6          = 224,
    ifDXBC7          = 225}
    ifLast           = 255
  );

  { Color value for 32 bit images.}
  TColor32 = UInt32;
  PColor32 = ^TColor32;

  { Color value for 64 bit images.}
  TColor64 = type Int64;
  PColor64 = ^TColor64;

  { Color record for 24 bit images, which allows access to individual color
    channels.}
  TColor24Rec = packed record
    case LongInt of
      0: (B, G, R: Byte);
      1: (Channels: array[0..2] of Byte);
  end;
  PColor24Rec = ^TColor24Rec;
  TColor24RecArray = array[0..MaxInt div SizeOf(TColor24Rec) - 1] of TColor24Rec;
  PColor24RecArray = ^TColor24RecArray;

  { Color record for 32 bit images, which allows access to individual color
    channels.}
  TColor32Rec = packed record
    case LongInt of
      0: (Color: TColor32);
      1: (B, G, R, A: Byte);
      2: (Channels: array[0..3] of Byte);
      3: (Color24Rec: TColor24Rec);
  end;
  PColor32Rec = ^TColor32Rec;
  TColor32RecArray = array[0..MaxInt div SizeOf(TColor32Rec) - 1] of TColor32Rec;
  PColor32RecArray = ^TColor32RecArray;

  { Color record for 48 bit images, which allows access to individual color
    channels.}
  TColor48Rec = packed record
    case LongInt of
      0: (B, G, R: Word);
      1: (Channels: array[0..2] of Word);
  end;
  PColor48Rec = ^TColor48Rec;
  TColor48RecArray = array[0..MaxInt div SizeOf(TColor48Rec) - 1] of TColor48Rec;
  PColor48RecArray = ^TColor48RecArray;

  { Color record for 64 bit images, which allows access to individual color
    channels.}
  TColor64Rec = packed record
    case LongInt of
      0: (Color: TColor64);
      1: (B, G, R, A: Word);
      2: (Channels: array[0..3] of Word);
      3: (Color48Rec: TColor48Rec);
  end;
  PColor64Rec = ^TColor64Rec;
  TColor64RecArray = array[0..MaxInt div SizeOf(TColor64Rec) - 1] of TColor64Rec;
  PColor64RecArray = ^TColor64RecArray;

  { Color record for 96 bit floating point images, which allows access to
    individual color channels.}
  TColor96FPRec = packed record
    case Integer of
      0: (B, G, R: Single);
      1: (Channels: array[0..2] of Single);
  end;
  PColor96FPRec = ^TColor96FPRec;
  TColor96FPRecArray = array[0..MaxInt div SizeOf(TColor96FPRec) - 1] of TColor96FPRec;
  PColor96FPRecArray = ^TColor96FPRecArray;

  { Color record for 128 bit floating point images, which allows access to
    individual color channels.}
  TColorFPRec = packed record
    case LongInt of
      0: (B, G, R, A: Single);
      1: (Channels: array[0..3] of Single);
      2: (Color96Rec: TColor96FPRec);
  end;
  PColorFPRec = ^TColorFPRec;
  TColorFPRecArray = array[0..MaxInt div SizeOf(TColorFPRec) - 1] of TColorFPRec;
  PColorFPRecArray = ^TColorFPRecArray;

  { 16 bit floating-point value. It has 1 sign bit, 5 exponent bits,
    and 10 mantissa bits.}
  THalfFloat = type Word;
  PHalfFloat = ^THalfFloat;

  { Color record for 64 bit floating point images, which allows access to
    individual color channels.}
  TColorHFRec = packed record
    case LongInt of
      0: (B, G, R, A: THalfFloat);
      1: (Channels: array[0..3] of THalfFloat);
  end;
  PColorHFRec = ^TColorHFRec;
  TColorHFRecArray = array[0..MaxInt div SizeOf(TColorHFRec) - 1] of TColorHFRec;
  PColorHFRecArray = ^TColorHFRecArray;

  { Palette for indexed mode images with 32 bit colors.}
  TPalette32 = TColor32RecArray;
  TPalette32Size256 = array[0..255] of TColor32Rec;
  PPalette32 = ^TPalette32;

  { Palette for indexd mode images with 24 bit colors.}
  TPalette24 = TColor24RecArray;
  TPalette24Size256 = array[0..255] of TColor24Rec;
  PPalette24 = ^TPalette24;

  { Record that stores single image data and information describing it.}
  TImageData = packed record
    Width: LongInt;       // Width of image in pixels
    Height: LongInt;      // Height of image in pixels
    Format: TImageFormat; // Data format of image
    Size: LongInt;        // Size of image bits in Bytes
    Bits: Pointer;        // Pointer to memory containing image bits
    Palette: PPalette32;  // Image palette for indexed images
    Tag: Pointer;         // User data
  end;
  PImageData = ^TImageData;

  { Pixel format information used in conversions to/from 16 and 8 bit ARGB
    image formats.}
  TPixelFormatInfo = packed record
    ABitCount, RBitCount, GBitCount, BBitCount: Byte;
    ABitMask, RBitMask, GBitMask, BBitMask: UInt32;
    AShift, RShift, GShift, BShift: Byte;
    ARecDiv, RRecDiv, GRecDiv, BRecDiv: Byte;
  end;
  PPixelFormatInfo = ^TPixelFormatInfo;

  PImageFormatInfo = ^TImageFormatInfo;

  { Look at TImageFormatInfo.GetPixelsSize for details.}
  TFormatGetPixelsSizeFunc = function(Format: TImageFormat; Width,
    Height: LongInt): LongInt;
  { Look at TImageFormatInfo.CheckDimensions for details.}
  TFormatCheckDimensionsProc = procedure(Format: TImageFormat; var Width,
    Height: LongInt);
  { Function for getting pixel colors. Native pixel is read from Image and
    then translated to 32 bit ARGB.}
  TGetPixel32Func = function(Bits: Pointer; Info: PImageFormatInfo;
    Palette: PPalette32): TColor32Rec;
  { Function for getting pixel colors. Native pixel is read from Image and
    then translated to FP ARGB.}
  TGetPixelFPFunc = function(Bits: Pointer; Info: PImageFormatInfo;
    Palette: PPalette32): TColorFPRec;
  { Procedure for setting pixel colors. Input 32 bit ARGB color is translated to
    native format and then written to Image.}
  TSetPixel32Proc = procedure(Bits: Pointer; Info: PImageFormatInfo;
    Palette: PPalette32;const Color: TColor32Rec);
  { Procedure for setting pixel colors. Input FP ARGB color is translated to
    native format and then written to Image.}
  TSetPixelFPProc = procedure(Bits: Pointer; Info: PImageFormatInfo;
    Palette: PPalette32; const Color: TColorFPRec);

  { Additional information for each TImageFormat value.}
  TImageFormatInfo = packed record
    Format: TImageFormat;             // Format described by this record
    Name: array[0..15] of Char;       // Symbolic name of format
    BytesPerPixel: LongInt;           // Number of bytes per pixel (note: it is
                                      // 0 for formats where BitsPerPixel < 8 (e.g. DXT).
                                      // Use GetPixelsSize function to get size of
                                      // image data.
    ChannelCount: LongInt;            // Number of image channels (R, G, B, A, Gray)
    PaletteEntries: LongInt;          // Number of palette entries
    HasGrayChannel: Boolean;          // True if image has grayscale channel
    HasAlphaChannel: Boolean;         // True if image has alpha channel
    IsFloatingPoint: Boolean;         // True if image has floating point pixels
    UsePixelFormat: Boolean;          // True if image uses pixel format
    IsRBSwapped: Boolean;             // True if Red and Blue channels are swapped
                                      // e.g. A16B16G16R16 has IsRBSwapped True
    RBSwapFormat: TImageFormat;       // Indicates supported format with swapped
                                      // Red and Blue channels, ifUnknown if such
                                      // format does not exist
    IsIndexed: Boolean;               // True if image uses palette
    IsSpecial: Boolean;               // True if image is in special format
    IsPassthrough: Boolean;           // True if image is in passthrough program (Imaging
                                      // itself doesn't know how to decode and encode it -
                                      // complex texture compressions etc.)
    PixelFormat: PPixelFormatInfo;    // Pixel format structure
    GetPixelsSize: TFormatGetPixelsSizeFunc; // Returns size in bytes of
                                      // Width * Height pixels of image
    CheckDimensions: TFormatCheckDimensionsProc; // some formats have limited
                                      // values of Width and Height. This
                                      // procedure checks and changes dimensions
                                      // to be valid for given format.
    GetPixel32: TGetPixel32Func;      // 32bit ARGB pixel get function
    GetPixelFP: TGetPixelFPFunc;      // FP ARGB pixel get function
    SetPixel32: TSetPixel32Proc;      // 32bit ARGB pixel set procedure
    SetPixelFP: TSetPixelFPProc;      // FP ARGB pixel set procedure
    SpecialNearestFormat: TImageFormat; // Regular image format used when
                                      // compressing/decompressing special images
                                      // as source/target
  end;

  { Handle to list of image data records.}
  TImageDataList = Pointer;
  PImageDataList = ^TImageDataList;

  { Handle to input/output.}
  TImagingHandle = Pointer;

  { Filters used in functions that resize images or their portions.}
  TResizeFilter = (
    rfNearest  = 0,
    rfBilinear = 1,
    rfBicubic  = 2,
    rfLanczos  = 3);

  { Seek origin mode for IO function Seek.}
  TSeekMode = (
   smFromBeginning = 0,
   smFromCurrent   = 1,
   smFromEnd       = 2);

  TOpenMode = (
    omReadOnly  = 0, // Opens file for reading only
    omCreate    = 1, // Creates new file (overwriting any existing) and opens it for writing
    omReadWrite = 2  // Opens for reading and writing. Non existing file is created.
  );

  { IO functions used for reading and writing images from/to input/output.}
  TOpenProc = function(Source: PChar; Mode: TOpenMode): TImagingHandle; cdecl;
  TCloseProc = procedure(Handle: TImagingHandle); cdecl;
  TEofProc = function(Handle: TImagingHandle): Boolean; cdecl;
  TSeekProc = function(Handle: TImagingHandle; Offset: Int64; Mode: TSeekMode): Int64; cdecl;
  TTellProc = function(Handle: TImagingHandle): Int64; cdecl;
  TReadProc = function(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;
  TWriteProc = function(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt): LongInt; cdecl;


implementation

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - add lookup tables to pixel formats for fast conversions

  -- 0.80 -----------------------------------------------------
    - Dropped "patch version".

  -- 0.77.3 ---------------------------------------------------
    - IO functions now have 64bit sizes and offsets.

  -- 0.77.1 ---------------------------------------------------
    - Added Tag to TImageData for storing user data.
    - Added ImagingPNGZLibStrategy option.
    - Changed IO functions. Merged open functions to one
      and added third open mode R/W (for TIFF append etc.).
    - Added new image data formats and related structures:
      ifR32G32B32F, ifB32G32G32F.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Added ifBinary image format and ImagingBinaryThreshold option.
    - Lanczos filter added to TResizeFilter enum.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Added ifATI1N and ifATI2N image data formats.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added ifBTC image format and SpecialNearestFormat field
      to TImageFormatInfo.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added option constants for PGM and PPM file formats.
    - Added TPalette32Size256 and TPalette24Size256 types.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added ImagingVersionPatch constant so bug fix only releases
      can be distinguished from ordinary major/minor releases
    - renamed TPixelFormat to TPixelFormatInfo to avoid name collisions
      with Graphics.TPixelFormat
    - added new image data formats:  ifR16F, ifA16R16G16B16F,
      ifA16B16G16R16F
    - added pixel get/set function pointers to TImageFormatInfo
    - added 16bit half float type and color record
    - renamed TColorFRec to TColorFPRec (and related types too)

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added option ImagingMipMapFilter which now controls resampling filter
      used when generating mipmaps
    - added TResizeFilter type
    - added ChannelCount to TImageFormatInfo
    - added new option constants for MNG and JNG images

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - added RBSwapFormat to TImageFormatInfo for faster conversions
      between swapped formats (it just calls SwapChannels now if
      RBSwapFormat is not ifUnknown)
    - moved TImageFormatInfo and required types from Imaging unit
      here, removed TImageFormatShortInfo
    - added new options: ImagingLoadOverrideFormat, ImagingSaveOverrideFormat

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - new ImagingColorReductionMask option added
    - new image format added: ifA16Gray16

}

end.
