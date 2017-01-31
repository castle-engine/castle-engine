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

{ Composite (like DDS) image file format handling (TCompositeImage). }
unit CastleCompositeImage;

{$I castleconf.inc}

interface

uses Classes, CastleImages;

type
  EInvalidCompositeImage = class(EInvalidImageFormat);
  EInvalidDDS = class(EInvalidCompositeImage);

  { Type of data in a TCompositeImage file.
    This doesn't take into account mipmaps (they are orthogonal to types here). }
  TCompositeType = (
    ctTexture,
    ctCubeMap,
    ctVolume);

  { Cube map faces.
    Always interpreted in right-handed orientation (like for OpenGL or OpenGL ES)
    by our engine.
    Order matches the order of OpenGL constants
    GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z_ARB. }
  TCubeMapSide = (
    csPositiveX,
    csNegativeX,
    csPositiveY,
    csNegativeY,
    csPositiveZ,
    csNegativeZ);
  TCubeMapSides = set of TCubeMapSide;

  TCubeMapImages = array [TCubeMapSide] of TCastleImage;

  { Composite image file (like DDS). This supports image that can have mipmaps,
    can be 3D, and can be a cubemap. This is something more than our TCastleImage
    (or TEncodedImage), which can only be a single pixel matrix (it can be 3D,
    but it cannot be a cubemap or have mipmaps).

    In essence, this is a container for a sequence of simple images
    in the @link(Images) property.
    The interpretation of the image sequence
    depends on other fields: first of all @link(CompositeType) and @link(Mipmaps).

    Right now supports only the DDS file format, but eventually
    will support KTX too.

    The basic usage of this class is to load a file using LoadFromFile
    or LoadFromStream.

    Note that you can write (change) many properties of this class.
    This allows you to create, or load and edit, composite files.
    You can even later save the composite image back to the stream (like a file) by
    SaveToStream or SaveToFile. Be careful though: you're responsible then
    to set all properties to sensible values. For example, the length
    (and interpretation) of @link(Images) list is determined by other properties
    of this class, so be sure to set them all to something sensible. }
  TCompositeImage = class
  strict private
    type
      { DDS cube map sides.

        Compared with TCubeMapSide type, the meaning
        positive/negative Y faces is swapped.

        Reason: Cube map sides are named and written in DDS file
        in a way natural for DirectX, and DirectX has left-handed coordinate system,
        which means that one axis seems reverted when you want OpenGL right-handed
        coord system (like
        OpenGL, see http://opengl.org/registry/specs/ARB/texture_cube_map.txt).
        See [http://castle-engine.sourceforge.net/x3d_implementation_status.php#section_dds]
        for more. }
      TDDSCubeMapSide = (
        dcsPositiveX,
        dcsNegativeX,
        dcsPositiveY,
        dcsNegativeY,
        dcsPositiveZ,
        dcsNegativeZ);
      TDDSCubeMapSides = set of TDDSCubeMapSide;
    const
      { Convert TDDSCubeMapSide to TCubeMapSide. }
      DDSToCubeMapSide: array [TDDSCubeMapSide] of TCubeMapSide =
      ( csPositiveX,
        csNegativeX,
        csNegativeY,
        csPositiveY,
        csPositiveZ,
        csNegativeZ
      );
      { Convert TCubeMapSide to TDDSCubeMapSide. }
      DDSFromCubeMapSide: array [TCubeMapSide] of TDDSCubeMapSide =
      ( dcsPositiveX,
        dcsNegativeX,
        dcsNegativeY,
        dcsPositiveY,
        dcsPositiveZ,
        dcsNegativeZ
      );
    var
    FImages: TEncodedImageList;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FCompositeType: TCompositeType;
    FMipmaps: boolean;
    FMipmapsCount: Cardinal;
    FCubeMapSides: TDDSCubeMapSides;
    FDepth: Cardinal;
    FOwnsFirstImage: boolean;
    function GetImages(const Index: Integer): TEncodedImage;
    function GetCubeMapSides: TCubeMapSides;
    procedure SetCubeMapSides(const Value: TCubeMapSides);
  public
    { Some DDS files specify unknown GPU texture compression.
      To read them, set this to true and set AutomaticCompressionType. }
    class var AutomaticCompression: boolean;
    class var AutomaticCompressionType: TTextureCompression;

    constructor Create;
    destructor Destroy; override;

    { Images sequence stored in this composite file.

      This has always length > 0 when file is successfully loaded
      (that is, when LoadFromStream method finished without raising any
      exception). }
    property Images: TEncodedImageList read FImages;

    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;

    property CompositeType: TCompositeType read FCompositeType write FCompositeType;

    { Does this composite image contain mipmaps.
      If @true, then all @link(Images) are guaranteed to have sizes
      being power of 2. }
    property Mipmaps: boolean read FMipmaps write FMipmaps;
    { Mipmaps count.
      Always 1 when @link(Mipmaps) = @false, this is usually comfortable. }
    property MipmapsCount: Cardinal read FMipmapsCount write FMipmapsCount;

    { Present cube map sides.
      Valid only when image is loaded and is ctCubeMap. }
    property CubeMapSides: TCubeMapSides read GetCubeMapSides write SetCubeMapSides;

    { Depth of volume (3D) texture.
      Always 1 when CompositeType is not ctVolume, this is usually comfortable. }
    property Depth: Cardinal read FDepth write FDepth;

    { Return given side of cube map.
      Assumes CompositeType = ctCubeMap and CubeMapSides = all.

      Level is mipmap level. Pass 0 for base level.
      When not @link(Mipmaps), Level must be 0. }
    function CubeMapImage(const Side: TCubeMapSide;
      const Level: Cardinal = 0): TEncodedImage;

    { Load composite (DDS) image from any TStream.
      @raises(EInvalidCompositeImage In case of any error in the file data.) }
    procedure LoadFromStream(Stream: TStream; const URL: string = '');

    procedure LoadFromFile(const URL: string);

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const URL: string);

    { Close all loaded image data. Effectively, this releases all data
      loaded by LoadFromStream, reverting the object to the state right
      after creation. }
    procedure Close;

    { When @false, then closing this composite image will not free Images[0].
      Closing happens when you call the @link(Close) method or
      destructor of this object. When this is @false, you're responsible
      to storing and freeing Images[0] later yourself, or you'll get memory
      leaks. }
    property OwnsFirstImage: boolean read FOwnsFirstImage write FOwnsFirstImage
      default true;

    { Convert 3D images in @link(Images) list into a sequences of 2D images.
      Useful utility for 3d (volume) textures.

      Normal loading of 3D composite textures creates single TCastleImage (using Depth
      possibly > 1) for each mipmap level. Such TCastleImage with depth
      is comfortable if you want to load this 3d texture into OpenGL
      (as then the image data is just a continous memory area,
      loadable by glTexImage3d). But it's not comfortable if you want
      to display it using some 2D GUI. For example, it's not comfortable
      for image viewer like glViewImage.

      So this method will convert such TCastleImage instances (with Depth > 1)
      into a sequence of TCastleImage instances all with Depth = 1.
      This isn't difficult, memory contents on 3d TCastleImage may be splitted
      into many 2d TCastleImage instances without problems.

      Note that it's safe to do this before saving the image.
      SaveToFile/SaveToStream methods accept both layouts of images
      (because, as said, memory contents actually are the same before
      and after splitting).

      Note that this may free all Images (possibly even whole Images object),
      disregarding OwnsFirstImage (as it would be difficult, since
      it may or may not replace it with new images). }
    procedure Flatten3d;

    { Decompress texture images (if any) on the @link(Images) list,
      replacing them with uncompressed equivalents.
      This can be used to decompress textures compressed using
      GPU compression algorithms, see @link(TTextureCompression).
      See TGPUCompressedImage.Decompress.

      Just like @link(Flatten3d):
      Note that this may free all Images (possibly even whole Images object),
      disregarding OwnsFirstImage (as it would be difficult, since
      it may or may not replace it with new images).

      @raises(ECannotDecompressTexture If some image cannot be decompressed
        for any reason.) }
    procedure DecompressTexture;

    { Does this URL look like it contains composite (DDS, KTX...) contents.
      Guesses looking at filename extension. }
    class function MatchesURL(const URL: string): boolean;

    procedure AddCubeMapImages(const AImages: TCubeMapImages);
  end;

const
  AllCubeMapSides = [Low(TCubeMapSide) .. High(TCubeMapSide)];

  CompositeTypeToString: array [TCompositeType] of string =
  ( 'Texture', 'CubeMap', 'Volume' );

implementation

uses SysUtils, CastleUtils, CastleClassUtils, CastleLog, CastleStringUtils,
  CastleVectors, CastleDownload, CastleURIUtils;

{ ----------------------------------------------------------------------------
  Constants and types for DDS file handling.

  This is based on MS docs on
  http://msdn.microsoft.com/en-us/library/bb943991(VS.85).aspx
  and gimp-dds/gimp-dds-2.0.7/dds.h,
  from gimp-dds plugin source code
  from http://nifelheim.dyndns.org/~cocidius/dds/.

  (gimp-dds is GNU GPL >= 2, and our engine is on LGPL >= 2,
  but I think it's Ok --- I did't copy code, just constant values
  known from DDS spec, and converted to Pascal). }

const
  DDSD_CAPS                   = $00000001;
  DDSD_HEIGHT                 = $00000002;
  DDSD_WIDTH                  = $00000004;
  DDSD_PITCH                  = $00000008;
  DDSD_PIXELFORMAT            = $00001000;
  DDSD_MIPMAPCOUNT            = $00020000;
  DDSD_LINEARSIZE             = $00080000;
  DDSD_DEPTH                  = $00800000;

  DDPF_ALPHAPIXELS            = $00000001;
  DDPF_ALPHA                  = $00000002;
  DDPF_FOURCC                 = $00000004;
  DDPF_PALETTEINDEXED8        = $00000020;
  DDPF_RGB                    = $00000040;
  DDPF_LUMINANCE              = $00020000;

  DDSCAPS_COMPLEX             = $00000008;
  DDSCAPS_TEXTURE             = $00001000;
  DDSCAPS_MIPMAP              = $00400000;

  DDSCAPS2_CUBEMAP            = $00000200;
  DDSCAPS2_CUBEMAP_POSITIVEX  = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX  = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY  = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY  = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ  = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ  = $00008000;
  DDSCAPS2_VOLUME             = $00200000;

  { Special FourCC constants, indicating float textures,
    from OGRE OgreDDSCodec.cpp. }
  D3DFMT_R16F            = 111;
  D3DFMT_G16R16F         = 112;
  D3DFMT_A16B16G16R16F   = 113;
  D3DFMT_R32F            = 114;
  D3DFMT_G32R32F         = 115;
  D3DFMT_A32B32G32R32F   = 116;

  { Constants for DxgiFormat, see
    https://msdn.microsoft.com/en-us/library/windows/desktop/bb173059%28v=vs.85%29.aspx }
  DXGI_FORMAT_UNKNOWN                     = 0;
  DXGI_FORMAT_R32G32B32A32_TYPELESS       = 1;
  DXGI_FORMAT_R32G32B32A32_FLOAT          = 2;
  DXGI_FORMAT_R32G32B32A32_UINT           = 3;
  DXGI_FORMAT_R32G32B32A32_SINT           = 4;
  DXGI_FORMAT_R32G32B32_TYPELESS          = 5;
  DXGI_FORMAT_R32G32B32_FLOAT             = 6;
  DXGI_FORMAT_R32G32B32_UINT              = 7;
  DXGI_FORMAT_R32G32B32_SINT              = 8;
  DXGI_FORMAT_R16G16B16A16_TYPELESS       = 9;
  DXGI_FORMAT_R16G16B16A16_FLOAT          = 10;
  DXGI_FORMAT_R16G16B16A16_UNORM          = 11;
  DXGI_FORMAT_R16G16B16A16_UINT           = 12;
  DXGI_FORMAT_R16G16B16A16_SNORM          = 13;
  DXGI_FORMAT_R16G16B16A16_SINT           = 14;
  DXGI_FORMAT_R32G32_TYPELESS             = 15;
  DXGI_FORMAT_R32G32_FLOAT                = 16;
  DXGI_FORMAT_R32G32_UINT                 = 17;
  DXGI_FORMAT_R32G32_SINT                 = 18;
  DXGI_FORMAT_R32G8X24_TYPELESS           = 19;
  DXGI_FORMAT_D32_FLOAT_S8X24_UINT        = 20;
  DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS    = 21;
  DXGI_FORMAT_X32_TYPELESS_G8X24_UINT     = 22;
  DXGI_FORMAT_R10G10B10A2_TYPELESS        = 23;
  DXGI_FORMAT_R10G10B10A2_UNORM           = 24;
  DXGI_FORMAT_R10G10B10A2_UINT            = 25;
  DXGI_FORMAT_R11G11B10_FLOAT             = 26;
  DXGI_FORMAT_R8G8B8A8_TYPELESS           = 27;
  DXGI_FORMAT_R8G8B8A8_UNORM              = 28;
  DXGI_FORMAT_R8G8B8A8_UNORM_SRGB         = 29;
  DXGI_FORMAT_R8G8B8A8_UINT               = 30;
  DXGI_FORMAT_R8G8B8A8_SNORM              = 31;
  DXGI_FORMAT_R8G8B8A8_SINT               = 32;
  DXGI_FORMAT_R16G16_TYPELESS             = 33;
  DXGI_FORMAT_R16G16_FLOAT                = 34;
  DXGI_FORMAT_R16G16_UNORM                = 35;
  DXGI_FORMAT_R16G16_UINT                 = 36;
  DXGI_FORMAT_R16G16_SNORM                = 37;
  DXGI_FORMAT_R16G16_SINT                 = 38;
  DXGI_FORMAT_R32_TYPELESS                = 39;
  DXGI_FORMAT_D32_FLOAT                   = 40;
  DXGI_FORMAT_R32_FLOAT                   = 41;
  DXGI_FORMAT_R32_UINT                    = 42;
  DXGI_FORMAT_R32_SINT                    = 43;
  DXGI_FORMAT_R24G8_TYPELESS              = 44;
  DXGI_FORMAT_D24_UNORM_S8_UINT           = 45;
  DXGI_FORMAT_R24_UNORM_X8_TYPELESS       = 46;
  DXGI_FORMAT_X24_TYPELESS_G8_UINT        = 47;
  DXGI_FORMAT_R8G8_TYPELESS               = 48;
  DXGI_FORMAT_R8G8_UNORM                  = 49;
  DXGI_FORMAT_R8G8_UINT                   = 50;
  DXGI_FORMAT_R8G8_SNORM                  = 51;
  DXGI_FORMAT_R8G8_SINT                   = 52;
  DXGI_FORMAT_R16_TYPELESS                = 53;
  DXGI_FORMAT_R16_FLOAT                   = 54;
  DXGI_FORMAT_D16_UNORM                   = 55;
  DXGI_FORMAT_R16_UNORM                   = 56;
  DXGI_FORMAT_R16_UINT                    = 57;
  DXGI_FORMAT_R16_SNORM                   = 58;
  DXGI_FORMAT_R16_SINT                    = 59;
  DXGI_FORMAT_R8_TYPELESS                 = 60;
  DXGI_FORMAT_R8_UNORM                    = 61;
  DXGI_FORMAT_R8_UINT                     = 62;
  DXGI_FORMAT_R8_SNORM                    = 63;
  DXGI_FORMAT_R8_SINT                     = 64;
  DXGI_FORMAT_A8_UNORM                    = 65;
  DXGI_FORMAT_R1_UNORM                    = 66;
  DXGI_FORMAT_R9G9B9E5_SHAREDEXP          = 67;
  DXGI_FORMAT_R8G8_B8G8_UNORM             = 68;
  DXGI_FORMAT_G8R8_G8B8_UNORM             = 69;
  DXGI_FORMAT_BC1_TYPELESS                = 70;
  DXGI_FORMAT_BC1_UNORM                   = 71;
  DXGI_FORMAT_BC1_UNORM_SRGB              = 72;
  DXGI_FORMAT_BC2_TYPELESS                = 73;
  DXGI_FORMAT_BC2_UNORM                   = 74;
  DXGI_FORMAT_BC2_UNORM_SRGB              = 75;
  DXGI_FORMAT_BC3_TYPELESS                = 76;
  DXGI_FORMAT_BC3_UNORM                   = 77;
  DXGI_FORMAT_BC3_UNORM_SRGB              = 78;
  DXGI_FORMAT_BC4_TYPELESS                = 79;
  DXGI_FORMAT_BC4_UNORM                   = 80;
  DXGI_FORMAT_BC4_SNORM                   = 81;
  DXGI_FORMAT_BC5_TYPELESS                = 82;
  DXGI_FORMAT_BC5_UNORM                   = 83;
  DXGI_FORMAT_BC5_SNORM                   = 84;
  DXGI_FORMAT_B5G6R5_UNORM                = 85;
  DXGI_FORMAT_B5G5R5A1_UNORM              = 86;
  DXGI_FORMAT_B8G8R8A8_UNORM              = 87;
  DXGI_FORMAT_B8G8R8X8_UNORM              = 88;
  DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM  = 89;
  DXGI_FORMAT_B8G8R8A8_TYPELESS           = 90;
  DXGI_FORMAT_B8G8R8A8_UNORM_SRGB         = 91;
  DXGI_FORMAT_B8G8R8X8_TYPELESS           = 92;
  DXGI_FORMAT_B8G8R8X8_UNORM_SRGB         = 93;
  DXGI_FORMAT_BC6H_TYPELESS               = 94;
  DXGI_FORMAT_BC6H_UF16                   = 95;
  DXGI_FORMAT_BC6H_SF16                   = 96;
  DXGI_FORMAT_BC7_TYPELESS                = 97;
  DXGI_FORMAT_BC7_UNORM                   = 98;
  DXGI_FORMAT_BC7_UNORM_SRGB              = 99;
  DXGI_FORMAT_AYUV                        = 100;
  DXGI_FORMAT_Y410                        = 101;
  DXGI_FORMAT_Y416                        = 102;
  DXGI_FORMAT_NV12                        = 103;
  DXGI_FORMAT_P010                        = 104;
  DXGI_FORMAT_P016                        = 105;
  DXGI_FORMAT_420_OPAQUE                  = 106;
  DXGI_FORMAT_YUY2                        = 107;
  DXGI_FORMAT_Y210                        = 108;
  DXGI_FORMAT_Y216                        = 109;
  DXGI_FORMAT_NV11                        = 110;
  DXGI_FORMAT_AI44                        = 111;
  DXGI_FORMAT_IA44                        = 112;
  DXGI_FORMAT_P8                          = 113;
  DXGI_FORMAT_A8P8                        = 114;
  DXGI_FORMAT_B4G4R4A4_UNORM              = 115;

type
  TDDSPixelFormat = packed record
    Size: LongWord;
    Flags: LongWord;
    case Integer of
      0: ( FourCC: array [0 .. 3] of char;
           RGBBitCount: LongWord;
           RBitMask: LongWord;
           GBitMask: LongWord;
           BBitMask: LongWord;
           ABitMask: LongWord; );
      1: ( { Alternative FourCC view, as LongWord }
           FourCCLW: LongWord; )

  end;
  PDDSPixelFormat = ^TDDSPixelFormat;

  { Corresponds to DDS_HEADER (Direct3D 10),
    http://msdn.microsoft.com/en-us/library/bb943982(VS.85).aspx. }
  TDDSHeader = packed record
    Size: LongWord;
    Flags: LongWord;
    Height: LongWord;
    Width: LongWord;
    PitchOrLinearSize: LongWord;
    Depth: LongWord;
    MipMapCount: LongWord;
    Reserved: array [0 .. 10] of LongWord;
    PixelFormat: TDDSPixelFormat;
    Caps1: LongWord;
    Caps2: LongWord;
    ReservedCaps: array [0 .. 1] of LongWord;
    Reserved2: LongWord;
  end;

  { DDS header extension to handle resource arrays,
    DXGI pixel formats that don't map to the legacy Microsoft DirectDraw
    pixel format structures, and additional metadata.
    From https://msdn.microsoft.com/en-us/library/windows/desktop/bb943983%28v=vs.85%29.aspx }
  TDDSHeaderDxt10 = packed record
    DxgiFormat: LongWord;
    ResourceDimension: LongWord;
    MiscFlag: LongWord;
    ArraySize: LongWord;
    MiscFlags2: LongWord;
  end;

{ TDDSRowReader -------------------------------------------------------------- }

type
  TRGBAChannel = 0..3;

  TChannelInfo = record
    Mask: LongWord;
    { Shift (to the right, by ShiftR) to fit into Byte }
    ShiftToByte: Integer;
    { Shift (to the right, by ShiftR) to have the least significant bit at
      1st position. }
    ShiftToLeast: Integer;
    { How long is the sequence of 1 bits inside the mask. }
    MaskOnes: Cardinal;
  end;

  { Reads DDS image rows, loading them to temporary memory.

    After reading row (by ReadRow) you can uncompress it by
    using RGBA (returns value as 8-bit Byte, for Red, Green, Blue, Alpha
    according to Channel parameter)
    and NextPixel repeatedly.

    It's callers responsibility to make sure that you use
    RGBA only with channels for which masks are non-zero.

    This is not the most efficient reader (it reads rows to temporary memory,
    for starters), but it works for all covered uncompressed non-palette
    DDS pixel formats. For special pixel formats, we use specialized
    optimized readers in ReadOptimized_*. }
  TDDSRowReader = class
  private
    FPixelFormat: PDDSPixelFormat;
    FWidth, FRowBytePadding: Cardinal;
    Row: Pointer;
    Pixel: Pointer;
    PixelValue: LongWord;
    RowByteSize, PixelByteSize: Cardinal;
    Channels: array [TRGBAChannel] of TChannelInfo;
    procedure CalculatePixelValue;
  public
    constructor Create(const PixelFormat: PDDSPixelFormat;
      const Width, RowBytePadding: Cardinal);
    destructor Destroy; override;

    procedure ReadRow(Stream: TStream);
    procedure NextPixel;
    function RGBA(Channel: TRGBAChannel): Byte;
  end;

{ Shift bits to the right if Value is positive.
  Shift to the left if negative. }
function ShiftR(const Value: LongWord; const Shift: Integer): LongWord;
begin
  if Shift >= 0 then
    Result := Value shr Shift else
    Result := Value shl (-Shift);
end;

constructor TDDSRowReader.Create(const PixelFormat: PDDSPixelFormat;
  const Width, RowBytePadding: Cardinal);

  { Calculate TChannelInfo. }
  procedure CalculateChannelInfo(const Mask: LongWord; var Info: TChannelInfo);
  const
    High1 = LongWord(1) shl 31;
  var
    LeadingZeros, Ones: Integer;
    M: LongWord;
  begin
    Info.Mask := Mask;

    if Mask = 0 then
    begin
      Info.ShiftToByte := 0;
      Info.ShiftToLeast := 0;
      Info.MaskOnes := 0;
      Exit;
    end;

    M := Mask;

    LeadingZeros := 0;
    while M and High1 = 0 do
    begin
      Inc(LeadingZeros);
      M := M shl 1;
    end;

    Ones := 0;
    while M <> 0 do
    begin
      Inc(Ones);
      M := M shl 1;
    end;
    Info.MaskOnes := Ones;

    { So the mask in binary starts with LeadingZeros of 0,
      then some 1. After Ones digits, the mask is zero.

      (In other words, we have a sequence of Ones bits inside the Mask
      that start and end with digit 1. We could assume here that
      this sequence is just full of 1 (nothing produces DDS
      files with other values, that would be pretty strange).
      But that's not actually necessary, all we need is to know
      the position of the most and least significant 1 digit,
      and then the trivial equation below will work Ok. }

    Info.ShiftToByte := 24 - LeadingZeros;
    Info.ShiftToLeast := 32 - LeadingZeros - Ones;

    { Assert that after shifting, all bits above Byte are clear
      and the most significant bit of color is in the most significant bit
      of byte. }
    Assert(ShiftR(Mask, Info.ShiftToByte) and $FFFFFF80 = $80);
  end;

begin
  inherited Create;
  FPixelFormat := PixelFormat;
  FWidth := Width;
  FRowBytePadding := RowBytePadding;

  { We already checked in TCompositeImage.LoadFromStream that RGBitCount divides
    by 8 and is not zero. }
  PixelByteSize := FPixelFormat^.RGBBitCount div 8;

  if PixelByteSize > SizeOf(PixelValue) then
    raise EInvalidDDS.CreateFmt('Unsupported DDS pixel format: more than 32 bits per pixel (RGBitCount is %d). Please report with sample image',
      [FPixelFormat^.RGBBitCount]);

  RowByteSize := PixelByteSize * Width;
  Row := GetMem(RowByteSize);

  CalculateChannelInfo(FPixelFormat^.RBitMask, Channels[0]);
  CalculateChannelInfo(FPixelFormat^.GBitMask, Channels[1]);
  CalculateChannelInfo(FPixelFormat^.BBitMask, Channels[2]);
  CalculateChannelInfo(FPixelFormat^.ABitMask, Channels[3]);
end;

destructor TDDSRowReader.Destroy;
begin
  FreeMem(Row);
  inherited;
end;

procedure TDDSRowReader.CalculatePixelValue;
begin
  PixelValue := 0;

  { The tricky memory part: move the meaningful bytes of current pixel
    (under Pixel^) to appropriate part of PixelValue, such that
    PixelValue can be directly and'ed with DDS masks (in ChannelMask[]).
    The copied bytes are the least significant part of LongWord value
    (DDS masks are specified like that). }

  Move(Pixel^, Pointer(PtrUInt(PtrUInt(@PixelValue)
    {$ifdef ENDIAN_BIG} + SizeOf(PixelValue) - PixelByteSize {$endif} ))^,
    PixelByteSize);
end;

procedure TDDSRowReader.ReadRow(Stream: TStream);
begin
  Stream.ReadBuffer(Row^, RowByteSize);

  if FRowBytePadding <> 0 then
    Stream.Seek(FRowBytePadding, soFromCurrent);

  Pixel := Row;
  CalculatePixelValue;
end;

procedure TDDSRowReader.NextPixel;
begin
  PtrUInt(Pixel) += PixelByteSize;
  CalculatePixelValue;
end;

function TDDSRowReader.RGBA(Channel: TRGBAChannel): Byte;
begin
  with Channels[Channel] do
  begin
    Result := ShiftR(PixelValue and Mask, ShiftToByte);

    { What to with bits that are not set by PixelValue?
      That is, when the mask is less than 8 bits?

      It's important that pure white be preserved as pure white,
      and pure black as pure black. This is especially important for alpha
      channel (otherwise opaque image will suddenly turn into
      partially transparent, e.g. if you load image with 2 bits for alpha,
      and fill the remaining 6 bits with 0, then alpha = 11 (binary)
      will be turned into 11000000 (no longer completely opaque).)

      So it seems most sensible to fill the remaining bits with the contents
      of least-significant color bit. If this bit is zero, we actually
      have to do nothing, but when it's one --- we need to insert 1 bits
      into Result.

      This is done only when MaskOnes < 8, to optimize. }
    if (MaskOnes < 8) and
       (ShiftR(PixelValue, ShiftToLeast) and 1 <> 0) then
    begin
      { So the least-significant color bit is 1. }
      Result := Result or (ShiftR(not Mask, ShiftToByte) and $FF);
    end;
  end;
end;

{ TCompositeImage ------------------------------------------------------------------ }

constructor TCompositeImage.Create;
begin
  inherited;
  FOwnsFirstImage := true;
  FImages := TEncodedImageList.Create(false);
end;

destructor TCompositeImage.Destroy;
begin
  Close;
  FreeAndNil(FImages);
  inherited;
end;

procedure TCompositeImage.Close;
var
  I: Integer;
begin
  for I := 0 to Images.Count - 1 do
    if OwnsFirstImage or (I <> 0) then
      FPGObjectList_FreeAndNilItem(Images, I);
  Images.Count := 0;
end;

function TCompositeImage.GetImages(const Index: Integer): TEncodedImage;
begin
  Result := FImages[Index];
end;

function TCompositeImage.CubeMapImage(const Side: TCubeMapSide;
  const Level: Cardinal): TEncodedImage;
var
  Index: Integer;
begin
  Index := Ord(DDSFromCubeMapSide[Side]);
  if Mipmaps then
    Result := FImages[Cardinal(Index) * FMipmapsCount + Level] else
    Result := FImages[Index];
end;

procedure TCompositeImage.LoadFromStream(Stream: TStream; const URL: string);

  procedure CheckWarn(const Check: boolean; const Message: string);
  begin
    if not Check then
      WritelnWarning('DDS image', Message);
  end;

var
  Header: TDDSHeader;

  { Reading header initializes many instance fields:
    FWidth, FHeight,
    FMipmaps, FMipmapsCount,
    FCompositeType, FCubeMapSides, FDepth }
  procedure ReadHeader;
  var
    Magic: array [0 .. 3] of char;
  begin
    Stream.ReadBuffer(Magic, SizeOf(Magic));
    Check(Magic = 'DDS ', 'DDS file beginning (magic number) invalid, maybe this is not really a DDS file');

    Stream.ReadBuffer(Header, SizeOf(Header));
    Assert(SizeOf(Header) = 124); { this is actually constant, as it's part of file format spec... }
    Check(Header.Size = SizeOf(Header), 'DDS header size incorrect');
    Check(Header.Flags and DDSD_CAPS <> 0, 'Missing DDSD_CAPS');
    Check(Header.Flags and DDSD_HEIGHT <> 0, 'Missing DDSD_HEIGHT');
    Check(Header.Flags and DDSD_WIDTH <> 0, 'Missing DDSD_WIDTH');
    Check(Header.Flags and DDSD_PIXELFORMAT <> 0, 'Missing DDSD_PIXELFORMAT');

    FHeight := Header.Height;
    FWidth := Header.Width;

    { calculate FMipmaps, FMipmapsCount }
    FMipmaps :=  Header.Caps1 and DDSCAPS_MIPMAP <> 0;
    if FMipmaps then
    begin
      Check(Header.Flags and DDSD_MIPMAPCOUNT <> 0, 'Missing DDSD_MIPMAPCOUNT, but caps indicate that this DDS image has mipmaps');
      Check(Header.Caps1 and DDSCAPS_COMPLEX <> 0, 'Missing DDSCAPS_COMPLEX, but caps indicate that this DDS image has mipmaps');
      FMipmapsCount := Header.MipMapCount;
      Check(MipmapsCount > 0, 'Specified mipmaps, but mipmap count is zero');
      { ATI Compresonator always sets this flag, even though it does not create mipmaps,
        and source image has not power-of-two sizes. So turn off FMipmaps flag. }
      if FMipmapsCount = 1 then
        FMipmaps := false;
    end else
      FMipmapsCount := 1;

    Check(Header.Flags and DDSCAPS_TEXTURE <> 0, 'Missing DDSCAPS_TEXTURE');

    { May be changed later if volume texture }
    FDepth := 1;

    { calculate CompositeType }
    Check( (Header.Caps2 and DDSCAPS2_VOLUME = 0) or
           (Header.Caps2 and DDSCAPS2_CUBEMAP = 0),
      'DDS capabilities indicate CUBEMAP and VOLUME texture at the same time');
    if Header.Caps2 and DDSCAPS2_CUBEMAP <> 0 then
    begin
      FCompositeType := ctCubeMap;

      { calculate FCubeMapSides }
      FCubeMapSides := [];
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEX <> 0 then Include(FCubeMapSides, dcsPositiveX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEX <> 0 then Include(FCubeMapSides, dcsNegativeX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEY <> 0 then Include(FCubeMapSides, dcsPositiveY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEY <> 0 then Include(FCubeMapSides, dcsNegativeY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEZ <> 0 then Include(FCubeMapSides, dcsPositiveZ);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEZ <> 0 then Include(FCubeMapSides, dcsNegativeZ);
      Check(FCubeMapSides <> [], 'DDS file is a cube map, but all six sides of the cube are missing');
    end else
    if Header.Caps2 and DDSCAPS2_VOLUME <> 0 then
    begin
      FCompositeType := ctVolume;

      { calculate FDepth }
      Check(Header.Flags and DDSD_DEPTH <> 0, 'Missing DDSD_DEPTH, but DDS is a VOLUME texture');
      FDepth := Header.Depth;
    end else
      FCompositeType := ctTexture;
    if FCompositeType <> ctTexture then
      { This invalid situation happens on
        http://regedit.gamedev.pl/Download/Rozne/Tekstury%20narzedziowe/NoiseVolume.dds
        (from
        http://regedit.gamedev.pl/download.php5?x=Rozne%2FTekstury+narzedziowe) }
      CheckWarn(Header.Caps1 and DDSCAPS_COMPLEX <> 0, 'Missing DDSCAPS_COMPLEX, but caps indicate that this DDS image has cube map or volume');

    Check(Header.PixelFormat.Size = SizeOf(Header.PixelFormat), 'Incorrect size of DDS pixel format record');
  end;

var
  HeaderDxt10: TDDSHeaderDxt10;

  procedure ReadHeaderDxt10;
  begin
    if (Header.PixelFormat.Flags and DDPF_FOURCC <> 0) and
       (Header.PixelFormat.FourCC = 'DX10') then
      Stream.ReadBuffer(HeaderDxt10, SizeOf(HeaderDxt10)) else
      FillChar(HeaderDxt10, SizeOf(HeaderDxt10), #0);
  end;

  { Read actual image data, initializing FImages contents }
  procedure ReadImages;

    { Read a single, normal 2D (or 3D) image from DDS file. }
    function ReadImage(const Width, Height, Depth, MipmapLevel: Cardinal): TEncodedImage;

    type
      TUncompressedType = (
        utRGB_AlphaPossible,
        utGrayscale_AlphaPossible,
        utPureAlpha );

      procedure ReadUncompressed(const UncompressedType: TUncompressedType);
      var
        RowBytePadding: Integer;
        { Within ReadUncompressed, Result is always of TCastleImage class }
        Res: TCastleImage absolute Result;

        { A couple of optimized routines for image formats that
          closely match corresponding Images unit memory format are below.
          Optimized routines already know the Result memory format (image class)
          and file format (Header.PixelFormat), they don't have to check them.

          Names for optimized routines follow the naming of DDS ms docs.
          This means that they are actually inverted in memory,
          since DDS bit masks should be treated as little-endian.
          For example for RGB image, RBitMask = $ff0000 means that red is
          the 3rd (not 1st) byte...)

          That's why DDS format RGB8 must be inverted when writing to TRGBImage.
          The format matching exactly TRGBImage is actually named BGR8 in DDS.

          The optimized routines are much faster as they don't use TDDSRowReader,
          so they don't need a temporary row (they load directly into output
          image memory), they don't need any mask/shift operations for each channel
          of each pixel (as the pixel format already matches what is needed,
          eventual swaps BGR<->RGB are fast).
          Tests shown that optimized versions are 4-7 times faster than
          if TDDSRowReader would be used to read the same images.
        }

        procedure ReadOptimized_G8;
        var
          Y, Z: Integer;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(Res.RowPtr(Y, Z)^, Res.PixelSize * Width);
              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadOptimized_AG8;
        var
          Y, Z: Integer;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(Res.RowPtr(Y, Z)^, Res.PixelSize * Width);
              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadOptimized_RGB8;
        var
          X, Y, Z: Integer;
          Row: PVector3Byte;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Row := Res.RowPtr(Y, Z);
              Stream.ReadBuffer(Row^, Res.PixelSize * Width);

              { Now invert red and blue. (Since all masks are little-endian,
                RBitMask = $FF0000 means that red is the 3rd (not 1st) byte...) }
              for X := 0 to Width - 1 do
              begin
                SwapValues(Row^[2], Row^[0]);
                Inc(Row);
              end;

              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadOptimized_BGR8;
        var
          Y, Z: Integer;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(Res.RowPtr(Y, Z)^, Res.PixelSize * Width);
              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadOptimized_ARGB8;
        var
          X, Y, Z: Integer;
          Row: PVector4Byte;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Row := Res.RowPtr(Y, Z);
              Stream.ReadBuffer(Row^, Res.PixelSize * Width);

              { Now invert ARGB to ABGR. So swap red<->blue, alpha and green are Ok. }
              for X := 0 to Width - 1 do
              begin
                SwapValues(Row^[2], Row^[0]);
                Inc(Row);
              end;

              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadOptimized_ABGR8;
        var
          Y, Z: Integer;
        begin
          for Z := 0 to Depth - 1 do
            for Y := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(Res.RowPtr(Y, Z)^, Res.PixelSize * Width);
              if RowBytePadding <> 0 then
                Stream.Seek(RowBytePadding, soFromCurrent);
            end;
        end;

        procedure ReadToGrayscale;
        var
          Reader: TDDSRowReader;
          X, Y, Z: Integer;
          G: PByte;
        begin
          Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
          try
            for Z := 0 to Depth - 1 do
              for Y := Height - 1 downto 0 do
              begin
                Reader.ReadRow(Stream);
                G := Res.RowPtr(Y, Z);
                for X := 0 to Width - 1 do
                begin
                  G^ := Reader.RGBA(0);
                  Reader.NextPixel;
                  Inc(G);
                end;
              end;
          finally FreeAndNil(Reader) end;
        end;

        procedure ReadToGrayscaleAlpha;
        var
          Reader: TDDSRowReader;
          X, Y, Z: Integer;
          GA: PVector2Byte;
        begin
          Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
          try
            for Z := 0 to Depth - 1 do
              for Y := Height - 1 downto 0 do
              begin
                Reader.ReadRow(Stream);
                GA := Res.RowPtr(Y, Z);
                for X := 0 to Width - 1 do
                begin
                  GA^[0] := Reader.RGBA(0);
                  GA^[1] := Reader.RGBA(3);
                  Reader.NextPixel;
                  Inc(GA);
                end;
              end;
          finally FreeAndNil(Reader) end;
        end;

        procedure ReadToRGB;
        var
          Reader: TDDSRowReader;
          X, Y, Z: Integer;
          RGB: PVector3Byte;
        begin
          Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
          try
            for Z := 0 to Depth - 1 do
              for Y := Height - 1 downto 0 do
              begin
                Reader.ReadRow(Stream);
                RGB := Res.RowPtr(Y, Z);
                for X := 0 to Width - 1 do
                begin
                  RGB^[0] := Reader.RGBA(0);
                  RGB^[1] := Reader.RGBA(1);
                  RGB^[2] := Reader.RGBA(2);
                  Reader.NextPixel;
                  Inc(RGB);
                end;
              end;
          finally FreeAndNil(Reader) end;
        end;

        procedure ReadToRGBAlpha;
        var
          Reader: TDDSRowReader;
          X, Y, Z: Integer;
          RGBA: PVector4Byte;
        begin
          Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
          try
            for Z := 0 to Depth - 1 do
              for Y := Height - 1 downto 0 do
              begin
                Reader.ReadRow(Stream);
                RGBA := Res.RowPtr(Y, Z);
                for X := 0 to Width - 1 do
                begin
                  RGBA^[0] := Reader.RGBA(0);
                  RGBA^[1] := Reader.RGBA(1);
                  RGBA^[2] := Reader.RGBA(2);
                  RGBA^[3] := Reader.RGBA(3);
                  Reader.NextPixel;
                  Inc(RGBA);
                end;
              end;
          finally FreeAndNil(Reader) end;
        end;

        { Read alpha-only images (may be produced by GIMP-DDS) }
        procedure ReadToGrayscaleAlphaPure;
        var
          Reader: TDDSRowReader;
          X, Y, Z: Integer;
          GA: PVector2Byte;
        begin
          Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
          try
            for Z := 0 to Depth - 1 do
              for Y := Height - 1 downto 0 do
              begin
                Reader.ReadRow(Stream);
                GA := Res.RowPtr(Y, Z);
                for X := 0 to Width - 1 do
                begin
                  GA^[0] := 255;
                  GA^[1] := Reader.RGBA(3);
                  Reader.NextPixel;
                  Inc(GA);
                end;
              end;
          finally FreeAndNil(Reader) end;
        end;

      begin { ReadUncompressed }
        Check(Header.PixelFormat.RGBBitCount mod 8 = 0, 'Invalid DDS pixel format: only RGBBitCount being multiple of 8 is supported. Please report with sample image');
        Check(Header.PixelFormat.RGBBitCount > 0, 'Invalid DDS pixel format: RGBBitCount must be non-zero');

        { Header.PitchOrLinearSize for uncompressed texture may indicate
          row length (pitch) in bytes. This is useful to indicate padding of lines.
          I understand that otherwise I should assume lines are not padded? }
        if (Header.Flags and DDSD_PITCH <> 0) and
           (Header.PitchOrLinearSize <> 0) and
           (MipmapLevel = 0) then
          RowBytePadding := Max(Header.PitchOrLinearSize -
            (Header.PixelFormat.RGBBitCount div 8) * Width, 0) else
          RowBytePadding := 0;

        case UncompressedType of
          utRGB_AlphaPossible:
            if Header.PixelFormat.Flags and DDPF_ALPHAPIXELS = 0 then
            begin
              Result := TRGBImage.Create(Width, Height, Depth);

              if (Header.PixelFormat.RBitMask = $ff0000) and
                 (Header.PixelFormat.GBitMask = $00ff00) and
                 (Header.PixelFormat.BBitMask = $0000ff) then
                ReadOptimized_RGB8 else
              if (Header.PixelFormat.RBitMask = $0000ff) and
                 (Header.PixelFormat.GBitMask = $00ff00) and
                 (Header.PixelFormat.BBitMask = $ff0000) then
                ReadOptimized_BGR8 else
                ReadToRGB;
            end else
            begin
              Check(Header.PixelFormat.ABitMask <> 0, 'Invalid DDS pixel format: alpha channel flag specified (DDPF_ALPHAPIXELS), but alpha mask is zero');

              Result := TRGBAlphaImage.Create(Width, Height, Depth);

              if (Header.PixelFormat.RGBBitCount = 32) and
                 (Header.PixelFormat.ABitMask = $ff000000) and
                 (Header.PixelFormat.RBitMask = $00ff0000) and
                 (Header.PixelFormat.GBitMask = $0000ff00) and
                 (Header.PixelFormat.BBitMask = $000000ff) then
                ReadOptimized_ARGB8 else
              if (Header.PixelFormat.RGBBitCount = 32) and
                 (Header.PixelFormat.ABitMask = $ff000000) and
                 (Header.PixelFormat.RBitMask = $000000ff) and
                 (Header.PixelFormat.GBitMask = $0000ff00) and
                 (Header.PixelFormat.BBitMask = $00ff0000) then
                ReadOptimized_ABGR8 else
                ReadToRGBAlpha;
            end;
          utGrayscale_AlphaPossible:
            if Header.PixelFormat.Flags and DDPF_ALPHAPIXELS = 0 then
            begin
              Result := TGrayscaleImage.Create(Width, Height, Depth);

              if (Header.PixelFormat.RGBBitCount = 8) and
                 (Header.PixelFormat.RBitMask = $ff) then
                ReadOptimized_G8 else
                ReadToGrayscale;
            end else
            begin
              Check(Header.PixelFormat.ABitMask <> 0, 'Invalid DDS pixel format: alpha channel flag specified (DDPF_ALPHAPIXELS), but alpha mask is zero');

              Result := TGrayscaleAlphaImage.Create(Width, Height, Depth);

              if (Header.PixelFormat.RGBBitCount = 16) and
                 (Header.PixelFormat.ABitMask = $ff00) and
                 (Header.PixelFormat.RBitMask = $00ff) then
                ReadOptimized_AG8 else
                ReadToGrayscaleAlpha;
            end;
          utPureAlpha:
            begin
              Check(Header.PixelFormat.ABitMask <> 0, 'Invalid DDS pixel format: pure alpha expected, but alpha mask is zero');

              Result := TGrayscaleAlphaImage.Create(Width, Height, Depth);
              ReadToGrayscaleAlphaPure;
            end;
          else raise EInternalError.Create('UncompressedType?');
        end;
      end { ReadUncompressed };

      procedure ReadCompressed(Compression: TTextureCompression);
      var
        { Within ReadUncompressed, Result is always of TGPUCompressedImage class }
        Res: TGPUCompressedImage absolute Result;
      begin
        Result := TGPUCompressedImage.Create(Width, Height, Depth,
          Compression);
        try
          { check Header.PitchOrLinearSize vs Result.Size }
          if (Header.Flags and DDSD_LINEARSIZE <> 0) and
            { It seems there are textures with DDSD_LINEARSIZE set but
              PitchOrLinearSize 0, and we should ignore
              PitchOrLinearSize then (e.g. ~/images/dds_tests/greek_imperial_swordsman.tga.dds
              on chantal). Same for PitchOrLinearSize = -1
              (e.g. UberPack-1/Torque3D/levels/lonerock_island/
              inside UberPack-1 on opengameart.org). }
            (Header.PitchOrLinearSize <> 0) and
            (Header.PitchOrLinearSize <> High(LongWord)) and
            { check this only on base level }
            (MipmapLevel = 0) and
            (Header.PitchOrLinearSize <> Result.Size) and
            ( (Compression in [tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5]) or
              { it seems that pvrtc includes mipmaps sizes too? }
              ((Compression in [
                tcPvrtc1_2bpp_RGB,
                tcPvrtc1_4bpp_RGB,
                tcPvrtc1_2bpp_RGBA,
                tcPvrtc1_4bpp_RGBA,
                tcPvrtc2_4bpp,
                tcPvrtc2_2bpp]) and not FMipmaps)
            ) then
            WritelnWarning('DDS', Format('Incorrect size for GPU compressed texture (DDS says %d, calculated should be %d, compression is %s)',
              [Header.PitchOrLinearSize, Result.Size,
               TextureCompressionInfo[Compression].Name]));

          Stream.ReadBuffer(Res.RawPixels^, Res.Size);

          Res.FlipVertical;
        { on unhandled error, make sure to free result }
        except FreeAndNil(Result); raise; end;
      end;

    begin
      { There are five mutually exclusive types of DDS pixel format:
        - uncompressed non-palette (DDPF_RGB, optional DDPF_ALPHAPIXELS) or
        - uncompressed palette (DDPF_PALETTEINDEXED8) or
        - compressed (DDPF_FOURCC) or
        - uncompressed grayscale (DDPF_LUMINANCE, optional DDPF_ALPHAPIXELS,
          GIMP-DDS can write such images).
        - neighter of the above: only DDPF_ALPHAPIXELS (pure alpha,
          GIMP-DDS can write such images; actually, GIMP-DDS omits even
          DDPF_ALPHAPIXELS).
      }

      Result := nil;
      try
        if Header.PixelFormat.Flags and DDPF_RGB <> 0 then
        begin
          Check(Header.PixelFormat.Flags and DDPF_FOURCC = 0, 'Invalid DDS pixel format: both uncompressed (DDPF_RGB) and compressed (DDPF_FOURCC) flags specified');
          Check(Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0, 'Invalid DDS pixel format: both non-palette (DDPF_RGB) and palette (DDPF_PALETTEINDEXED8) flags specified');
          ReadUncompressed(utRGB_AlphaPossible);
        end else
        if Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 <> 0 then
        begin
          Assert(Header.PixelFormat.Flags and DDPF_RGB = 0);
          Check(Header.PixelFormat.Flags and DDPF_FOURCC = 0, 'Invalid DDS pixel format: both uncompressed (palette, DDPF_PALETTEINDEXED8) and compressed (DDPF_FOURCC) flags specified');

          raise EInvalidDDS.Create('TODO: Unsupported pixel format for DDS: palette images not supported now, please report with sample image');
        end else
        if Header.PixelFormat.Flags and DDPF_FOURCC <> 0 then
        begin
          Assert(Header.PixelFormat.Flags and DDPF_RGB = 0);
          Assert(Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0);

          if Header.PixelFormat.FourCC = 'DXT1' then
            { There's no way to recognize from DDS file header whether it uses
              or not some transparent pixels. (DDPF_ALPHAPIXELS is never
              specified for compressed formats.)
              Potentially, every DXT1 image may have some transparent pixels,
              so use tcDxt1_RGBA. }
            ReadCompressed(tcDxt1_RGBA) else
          if Header.PixelFormat.FourCC = 'DXT3' then
            ReadCompressed(tcDxt3) else
          if Header.PixelFormat.FourCC = 'DXT5' then
            ReadCompressed(tcDxt5) else
          if Header.PixelFormat.FourCC = 'ATC ' then
            ReadCompressed(tcATITC_RGB) else
          if Header.PixelFormat.FourCC = 'ATCA' then
            ReadCompressed(tcATITC_RGBA_ExplicitAlpha) else
          if Header.PixelFormat.FourCC = 'ATCI' then
            ReadCompressed(tcATITC_RGBA_InterpolatedAlpha) else
          if Header.PixelFormat.FourCC = 'PTC2' then
          begin
            { No way to detect tcPvrtc1_2bpp_RGB? }
            ReadCompressed(tcPvrtc1_2bpp_RGBA);
          end else
          if Header.PixelFormat.FourCC = 'PTC4' then
          begin
            { No way to detect tcPvrtc1_4bpp_RGB? }
            ReadCompressed(tcPvrtc1_4bpp_RGBA);
          end else
          if Header.PixelFormat.FourCC = 'DX10' then
          begin
            { Right now, this is only used by PvrTexTool when you compress
              to PVRTC2_* formats. Unfortunately HeaderDxt10.DxgiFormat is useless
              in this case, it's still zero. }
            if AutomaticCompression then
              ReadCompressed(AutomaticCompressionType) else
              raise EInvalidDDS.CreateFmt('Unknown texture format in DDS: FourCC is DX10 and DxgiFormat is %d. Assign TCompositeImage.AutomaticCompression and TCompositeImage.AutomaticCompressionType in the engine to override this.',
                [HeaderDxt10.DxgiFormat]);
          end else
          if (Header.PixelFormat.FourCCLW = D3DFMT_R16F) or
             (Header.PixelFormat.FourCCLW = D3DFMT_G16R16F) or
             (Header.PixelFormat.FourCCLW = D3DFMT_A16B16G16R16F) or
             (Header.PixelFormat.FourCCLW = D3DFMT_R32F) or
             (Header.PixelFormat.FourCCLW = D3DFMT_G32R32F) or
             (Header.PixelFormat.FourCCLW = D3DFMT_A32B32G32R32F) then
            raise EInvalidDDS.Create('Unsupported texture compression for DDS: FourCC indicates float texture, not supported yet') else
            raise EInvalidDDS.CreateFmt('Unsupported texture compression for DDS: FourCC is "%s%s%s%s"',
              [ SReadableForm(Header.PixelFormat.FourCC[0]),
                SReadableForm(Header.PixelFormat.FourCC[1]),
                SReadableForm(Header.PixelFormat.FourCC[2]),
                SReadableForm(Header.PixelFormat.FourCC[3]) ]);
        end else
        if Header.PixelFormat.Flags and DDPF_LUMINANCE <> 0 then
        begin
          { Uncompressed grayscale image (possibly with alpha). }
          Check(Header.PixelFormat.RBitMask <> 0,
            'Invalid DDS pixel format: luminance image (DDPF_LUMINANCE flag) specified, but Red mask is zero');
          Check(
            ( (Header.PixelFormat.RBitMask = Header.PixelFormat.GBitMask) and
              (Header.PixelFormat.RBitMask = Header.PixelFormat.BBitMask) ) or
            ( (Header.PixelFormat.GBitMask = 0) and
              (Header.PixelFormat.BBitMask = 0) ),
            'Invalid DDS pixel format: luminance image (DDPF_LUMINANCE flag) specified, so Red = Green = Blue masks should be equal or Green = Blue = should be zero, but they are not');
          ReadUncompressed(utGrayscale_AlphaPossible);
        end else
        begin
          { GIMP-DDS plugin doesn't set DDPF_ALPHAPIXELS, but this is wrong IMO,
            so I warn about it. }
          CheckWarn(Header.PixelFormat.Flags and DDPF_ALPHAPIXELS <> 0, 'Invalid DDS pixel format: no flag specified, even not DDPF_ALPHAPIXELS. We will assume this is alpha-only image, as GIMP-DDS plugin can write such files');
          Check(
            (Header.PixelFormat.RBitMask = 0) and
            (Header.PixelFormat.GBitMask = 0) and
            (Header.PixelFormat.BBitMask = 0),
            'Invalid DDS pixel format: pure alpha image (only DDPF_ALPHAPIXELS flag) specified, but Red / Green / Blue makss are not zero');
          ReadUncompressed(utPureAlpha);
        end;
      except FreeAndNil(Result); raise end;

      // useful to see this URL in logs, TextureMemoryProfiler dumps etc.
      Result.URL := URL + '[subimage]';
    end { ReadImage };

    procedure AllocateImages(const Count: Cardinal);
    var
      I: Integer;
    begin
      Images.Count := Count;
      { zero memory, to allow easy deallocation in case an exception will
        be raised during image reading. Although FGL generics zero the new
        memory anyway, so it can be removed later (if this zeroing behavior
        is guaranteed forever). }
      for I := 0 to Integer(Count) - 1 do
        Images[I] := nil;
    end;

  var
    W, H, D: Cardinal;
    I: Integer;
    Side: TDDSCubeMapSide;
  begin
    { Check that Width/Height are power of two, this is needed to make
      sure images reading code is sensible (otherwise, there's no way
      to tell what are the sizes of following images). }
    if FMipmaps then
    begin
      if (not IsPowerOf2(Width)) or
         (not IsPowerOf2(Height)) then
        raise EInvalidDDS.CreateFmt('DDS image has mipmaps, but width or height are not a power of 2: %d x %d', [Width, Height]);
    end;

    case CompositeType of
      ctTexture:
        begin
          if Mipmaps then
          begin
            AllocateImages(FMipmapsCount);
            W := Width;
            H := Height;
            for I := 0 to FMipmapsCount - 1 do
            begin
              FImages[I] := ReadImage(W, H, 1, I);
              W := Max(1, W div 2);
              H := Max(1, H div 2);
            end;
          end else
          begin
            AllocateImages(1);
            FImages[0] := ReadImage(Width, Height, 1, 0);
          end;
        end;
      ctCubeMap:
        begin
          for Side := Low(Side) to High(Side) do
            if Side in FCubeMapSides then
            begin
              if Mipmaps then
              begin
                W := Width;
                H := Height;
                for I := 0 to FMipmapsCount - 1 do
                begin
                  Images.Add(ReadImage(W, H, 1, I));
                  W := Max(1, W div 2);
                  H := Max(1, H div 2);
                end;
              end else
              begin
                Images.Add(ReadImage(Width, Height, 1, 0));
              end;
            end;
        end;
      ctVolume:
        begin
          if Mipmaps then
          begin
            W := Width;
            H := Height;
            D := Depth;
            for I := 0 to FMipmapsCount - 1 do
            begin
              Images.Add(ReadImage(W, H, D, I));
              W := Max(1, W div 2);
              H := Max(1, H div 2);
              D := Max(1, D div 2);
            end;
          end else
          begin
            Images.Add(ReadImage(Width, Height, Depth, 0));
          end;
        end;
      else raise EInternalError.Create('CompositeType?');
    end;
  end { ReadImages };

begin
  try
    Close;
    ReadHeader;
    ReadHeaderDxt10;
    ReadImages;
  except
    { EReadError is raised by Stream.ReadBuffer when it can't read
      specified number of bytes }
    on E: EReadError do raise EInvalidDDS.Create('Read error: ' + E.Message);
    on E: ECheckFailed do raise EInvalidDDS.Create('Wrong DDS file: ' + E.Message);
  end;
end;

procedure TCompositeImage.LoadFromFile(const URL: string);
var
  S: TStream;
begin
  S := Download(URL, [soForceMemoryStream]);
  try
    LoadFromStream(S, URL);
  finally FreeAndNil(S) end;
end;

procedure TCompositeImage.SaveToStream(Stream: TStream);

  procedure WriteHeader;
  const
    Magic: array [0 .. 3] of char = 'DDS ';
  var
    Header: TDDSHeader;
  begin
    Stream.WriteBuffer(Magic, SizeOf(Magic));

    { initially fill Header with zeros, to avoid writing memory garbage
      (which would be potential security risk) to file. }
    FillChar(Header, SizeOf(Header), 0);

    Header.Size := SizeOf(Header);
    Header.Flags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT
      or DDSCAPS_TEXTURE;
    Header.Height := Height;
    Header.Width := Width;
    Header.Caps1 := 0; { for starters }

    if Mipmaps then
    begin
      Header.Caps1 := Header.Caps1 or DDSCAPS_COMPLEX or DDSCAPS_MIPMAP;
      Header.Flags := Header.Flags or DDSD_MIPMAPCOUNT;
      Header.MipMapCount := MipmapsCount;
    end;

    case CompositeType of
      ctTexture: ;
      ctCubeMap:
        begin
          Header.Caps1 := Header.Caps1 or DDSCAPS_COMPLEX;
          Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP;

          if dcsPositiveX in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_POSITIVEX;
          if dcsNegativeX in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_NEGATIVEX;
          if dcsPositiveY in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_POSITIVEY;
          if dcsNegativeY in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_NEGATIVEY;
          if dcsPositiveZ in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_POSITIVEZ;
          if dcsNegativeZ in FCubeMapSides then Header.Caps2 := Header.Caps2 or DDSCAPS2_CUBEMAP_NEGATIVEZ;
        end;
      ctVolume:
        begin
          Header.Caps1 := Header.Caps1 or DDSCAPS_COMPLEX;
          Header.Caps2 := Header.Caps2 or DDSCAPS2_VOLUME;
          Header.Flags := Header.Flags or DDSD_DEPTH;
          Header.Depth := Depth;
        end;
      else raise EInternalError.Create('CompositeType');
    end;

    if Images[0] is TCastleImage then
    begin
      { For uncompressed image, PitchOrLinearSize is row length }
      Header.PitchOrLinearSize := Width * TCastleImage(Images[0]).PixelSize;
      Header.Flags := Header.Flags or DDSD_PITCH;
    end else
    if Images[0] is TGPUCompressedImage then
    begin
      { For compressed image, PitchOrLinearSize is image length }
      Header.PitchOrLinearSize := TGPUCompressedImage(Images[0]).Size;
      Header.Flags := Header.Flags or DDSD_LINEARSIZE;
    end else
      raise Exception.CreateFmt('Cannot save image class %s to DDS file', [Images[0].ClassName]);

    Header.PixelFormat.Size := SizeOf(Header.PixelFormat);
    Header.PixelFormat.Flags := 0; { for starters }

    if Images[0] is TGrayscaleImage then
    begin
      Header.PixelFormat.Flags := DDPF_LUMINANCE;
      Header.PixelFormat.RGBBitCount := 8;
      Header.PixelFormat.RBitMask := $ff;
      Header.PixelFormat.GBitMask := $ff;
      Header.PixelFormat.BBitMask := $ff;
      Header.PixelFormat.ABitMask := 0;
    end else
    if Images[0] is TGrayscaleAlphaImage then
    begin
      Header.PixelFormat.Flags := DDPF_LUMINANCE or DDPF_ALPHAPIXELS;
      Header.PixelFormat.RGBBitCount := 16;
      Header.PixelFormat.RBitMask := $00ff;
      Header.PixelFormat.GBitMask := $00ff;
      Header.PixelFormat.BBitMask := $00ff;
      Header.PixelFormat.ABitMask := $ff00;
    end else
    if Images[0] is TRGBImage then
    begin
      Header.PixelFormat.Flags := DDPF_RGB;
      Header.PixelFormat.RGBBitCount := 24;
      Header.PixelFormat.RBitMask := $000000ff;
      Header.PixelFormat.GBitMask := $0000ff00;
      Header.PixelFormat.BBitMask := $00ff0000;
      Header.PixelFormat.ABitMask := 0;
    end else
    if Images[0] is TRGBAlphaImage then
    begin
      Header.PixelFormat.Flags := DDPF_RGB or DDPF_ALPHAPIXELS;
      Header.PixelFormat.RGBBitCount := 32;
      Header.PixelFormat.RBitMask := $000000ff;
      Header.PixelFormat.GBitMask := $0000ff00;
      Header.PixelFormat.BBitMask := $00ff0000;
      Header.PixelFormat.ABitMask := $ff000000;
    end else
    if Images[0] is TGPUCompressedImage then
    begin
      Header.PixelFormat.Flags := DDPF_FOURCC;
      case TGPUCompressedImage(Images[0]).Compression of
        tcDxt1_RGB,
        tcDxt1_RGBA : Header.PixelFormat.FourCC := 'DXT1';
        tcDxt3:       Header.PixelFormat.FourCC := 'DXT3';
        tcDxt5:       Header.PixelFormat.FourCC := 'DXT5';
        tcPvrtc1_2bpp_RGB,
        tcPvrtc1_2bpp_RGBA: Header.PixelFormat.FourCC := 'PTC2';
        tcPvrtc1_4bpp_RGB,
        tcPvrtc1_4bpp_RGBA: Header.PixelFormat.FourCC := 'PTC4';
        tcATITC_RGB                   : Header.PixelFormat.FourCC := 'ATC ';
        tcATITC_RGBA_ExplicitAlpha    : Header.PixelFormat.FourCC := 'ATCA';
        tcATITC_RGBA_InterpolatedAlpha: Header.PixelFormat.FourCC := 'ATCI';
        else raise EImageSaveError.CreateFmt('When saving DDS: Cannot save to DDS with compression %s',
          [TextureCompressionInfo[TGPUCompressedImage(Images[0]).Compression].Name]);
      end;
    end else
      raise Exception.CreateFmt('Unable to save image class %s to DDS image',
        [Images[0].ClassName]);

    Stream.WriteBuffer(Header, SizeOf(Header));
  end;

  procedure WriteImages;

    procedure WriteUncompressedImage(Image: TCastleImage);
    var
      Z, Y: Integer;
    begin
      for Z := 0 to Image.Depth - 1 do
        { We have to invert rows order when saving to DDS }
        for Y := Image.Height - 1 downto 0 do
          Stream.WriteBuffer(Image.RowPtr(Y, Z)^, Image.Width * Image.PixelSize);
    end;

    procedure WriteCompressedImage(Image: TGPUCompressedImage);
    var
      Temp: TGPUCompressedImage;
    begin
      Temp := Image.MakeCopy;
      try
        { invert rows when saving to DDS }
        Temp.FlipVertical;
        Stream.WriteBuffer(Temp.RawPixels^, Temp.Size);
      finally FreeAndNil(Temp) end;
    end;

  var
    I: Integer;
  begin
    for I := 0 to Images.Count - 1 do
      if Images[I] is TCastleImage then
        WriteUncompressedImage(TCastleImage(Images[I])) else
      begin
        Assert(Images[I] is TGPUCompressedImage);
        WriteCompressedImage(TGPUCompressedImage(Images[I]));
      end;
  end;

begin
  Assert(Images.Count > 0, 'Images count must be > 0 when saving DDS image');
  WriteHeader;
  WriteImages;
end;

procedure TCompositeImage.SaveToFile(const URL: string);
var
  S: TStream;
begin
  S := URLSaveStream(URL);
  try
    SaveToStream(S);
  finally FreeAndNil(S) end;
end;

class function TCompositeImage.MatchesURL(const URL: string): boolean;
begin
  Result := URIMimeType(URL) = 'image/x-dds';
end;

procedure TCompositeImage.Flatten3d;
var
  NewImages: TEncodedImageList;
  OldImage, NewImage: TCastleImage;
  I, J: Integer;
begin
  if (CompositeType = ctVolume) and (Depth > 1) then
  begin
    NewImages := TEncodedImageList.Create(false);

    for I := 0 to Images.Count - 1 do
    begin
      if not (Images[I] is TCastleImage) then
        raise Exception.CreateFmt('Cannot do Flatten3d on this image class: %s',
          [Images[I].ClassName]);
      OldImage := TCastleImage(Images[I]);

      for J := 0 to OldImage.Depth - 1 do
      begin
        NewImage := TCastleImageClass(OldImage.ClassType).Create(
          OldImage.Width, OldImage.Height, 1);
        Move(OldImage.PixelPtr(0, 0, J)^, NewImage.RawPixels^,
          OldImage.Width * OldImage.Height * OldImage.PixelSize);
        NewImages.Add(NewImage);
      end;
    end;

    Close;
    FreeAndNil(FImages);
    FImages := NewImages;
  end;
end;

procedure TCompositeImage.DecompressTexture;
var
  OldImage: TGPUCompressedImage;
  I: Integer;
begin
  for I := 0 to Images.Count - 1 do
    if Images[I] is TGPUCompressedImage then
    begin
      OldImage := TGPUCompressedImage(Images[I]);
      Images[I] := OldImage.Decompress;
      FreeAndNil(OldImage);
    end;
end;

function TCompositeImage.GetCubeMapSides: TCubeMapSides;
var
  DDSSide: TDDSCubeMapSide;
begin
  Result := [];
  for DDSSide in FCubeMapSides do
    Include(Result, DDSToCubeMapSide[DDSSide]);
end;

procedure TCompositeImage.SetCubeMapSides(const Value: TCubeMapSides);
var
  Side: TCubeMapSide;
begin
  FCubeMapSides := [];
  for Side in Value do
    Include(FCubeMapSides, DDSFromCubeMapSide[Side]);
end;

procedure TCompositeImage.AddCubeMapImages(const AImages: TCubeMapImages);
var
  StartIndex: Integer;
begin
  StartIndex := Images.Count;
  Images.Count := StartIndex + 6;
  { note that we use DDSFromCubeMapSide on the left side,
    so that Images order is like in TDDSCubeMapSide enum. }
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveX])] := AImages[csPositiveX];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeX])] := AImages[csNegativeX];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveY])] := AImages[csPositiveY];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeY])] := AImages[csNegativeY];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csPositiveZ])] := AImages[csPositiveZ];
  Images[StartIndex + Ord(DDSFromCubeMapSide[csNegativeZ])] := AImages[csNegativeZ];
end;

end.
