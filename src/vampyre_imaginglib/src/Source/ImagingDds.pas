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

{ This unit contains image format loader/saver for DirectDraw Surface images.}
unit ImagingDds;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingUtility, ImagingFormats;

type
  { Class for loading and saving Microsoft DirectDraw surfaces.
    It can load/save all D3D formats which have corresponding
    TImageFormat. It supports plain textures, cube textures and
    volume textures, all of these can have mipmaps. It can also
    load some formats which have no exact TImageFormat, but can be easily
    converted to one (bump map formats, etc.).
    You can get some information about last loaded DDS file by calling
    GetOption with ImagingDDSLoadedXXX options and you can set some
    saving options by calling SetOption with ImagingDDSSaveXXX or you can
    simply use properties of this class.
    Note that when saving cube maps and volumes input image array must contain
    at least number of images to build cube/volume based on current
    Depth and MipMapCount settings.}
  TDDSFileFormat = class(TImageFileFormat)
  private
    FLoadedCubeMap: LongBool;
    FLoadedVolume: LongBool;
    FLoadedMipMapCount: LongInt;
    FLoadedDepth: LongInt;
    FSaveCubeMap: LongBool;
    FSaveVolume: LongBool;
    FSaveMipMapCount: LongInt;
    FSaveDepth: LongInt;
    procedure ComputeSubDimensions(Idx, Width, Height, MipMaps, Depth: LongInt;
      IsCubeMap, IsVolume: Boolean; var CurWidth, CurHeight: LongInt);
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
  published
    { True if last loaded DDS file was cube map.}
    property LoadedCubeMap: LongBool read FLoadedCubeMap write FLoadedCubeMap;
    { True if last loaded DDS file was volume texture.}
    property LoadedVolume: LongBool read FLoadedVolume write FLoadedVolume;
    { Number of mipmap levels of last loaded DDS image.}
    property LoadedMipMapCount: LongInt read FLoadedMipMapCount write FLoadedMipMapCount;
    { Depth (slices of volume texture or faces of cube map) of last loaded DDS image.}
    property LoadedDepth: LongInt read FLoadedDepth write FLoadedDepth;
    { True if next DDS file to be saved should be stored as cube map.}
    property SaveCubeMap: LongBool read FSaveCubeMap write FSaveCubeMap;
    { True if next DDS file to be saved should be stored as volume texture.}
    property SaveVolume: LongBool read FSaveVolume write FSaveVolume;
    { Sets the number of mipmaps which should be stored in the next saved DDS file.
      Only applies to cube maps and volumes, ordinary 2D textures save all
      levels present in input.}
    property SaveMipMapCount: LongInt read FSaveMipMapCount write FSaveMipMapCount;
    { Sets the depth (slices of volume texture or faces of cube map)
      of the next saved DDS file.}
    property SaveDepth: LongInt read FSaveDepth write FSaveDepth;
  end;

const
  { DDS related metadata Ids }

  { DXGI format of textures stored in DDS files with DX10 extension. Type is
    Enum (value corresponding to DXGI_FORMAT enum from DX SDK).}
  SMetaDdsDxgiFormat = 'DdsDxgiFormat';
  { Number of mipmaps for each main image in DDS file.}
  SMetaDdsMipMapCount = 'DdsMipMapCount';
  { Texture array size stored in DDS file (DX10 extension).}
  SMetaDdsArraySize = 'DdsArraySize';

implementation

const
  SDDSFormatName = 'DirectDraw Surface';
  SDDSMasks      = '*.dds';
  DDSSupportedFormats: TImageFormats = [ifR8G8B8, ifA8R8G8B8, ifX8R8G8B8,
    ifA1R5G5B5, ifA4R4G4B4, ifX1R5G5B5, ifX4R4G4B4, ifR5G6B5, ifA16B16G16R16,
    ifR32F, ifA32B32G32R32F, ifR16F, ifA16B16G16R16F, ifR3G3B2, ifGray8, ifA8Gray8,
    ifGray16, ifDXT1, ifDXT3, ifDXT5, ifATI1N, ifATI2N];

const
  { Four character codes.}
  DDSMagic    = UInt32(Byte('D') or (Byte('D') shl 8) or (Byte('S') shl 16) or
    (Byte(' ') shl 24));
  FOURCC_DXT1 = UInt32(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or
    (Byte('1') shl 24));
  FOURCC_DXT3 = UInt32(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or
    (Byte('3') shl 24));
  FOURCC_DXT5 = UInt32(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or
    (Byte('5') shl 24));
  FOURCC_ATI1 = UInt32(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or
    (Byte('1') shl 24));
  FOURCC_ATI2 = UInt32(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or
    (Byte('2') shl 24));
  FOURCC_DX10 = UInt32(Byte('D') or (Byte('X') shl 8) or (Byte('1') shl 16) or
    (Byte('0') shl 24));

  { Some D3DFORMAT values used in DDS files as FourCC value.}
  D3DFMT_A16B16G16R16  = 36;
  D3DFMT_R32F          = 114;
  D3DFMT_A32B32G32R32F = 116;
  D3DFMT_R16F          = 111;
  D3DFMT_A16B16G16R16F = 113;

  { Constants used by TDDSurfaceDesc2.Flags.}
  DDSD_CAPS            = $00000001;
  DDSD_HEIGHT          = $00000002;
  DDSD_WIDTH           = $00000004;
  DDSD_PITCH           = $00000008;
  DDSD_PIXELFORMAT     = $00001000;
  DDSD_MIPMAPCOUNT     = $00020000;
  DDSD_LINEARSIZE      = $00080000;
  DDSD_DEPTH           = $00800000;

  { Constants used by TDDSPixelFormat.Flags.}
  DDPF_ALPHAPIXELS     = $00000001;    // used by formats which contain alpha
  DDPF_FOURCC          = $00000004;    // used by DXT and large ARGB formats
  DDPF_RGB             = $00000040;    // used by RGB formats
  DDPF_LUMINANCE       = $00020000;    // used by formats like D3DFMT_L16
  DDPF_BUMPLUMINANCE   = $00040000;    // used by mixed signed-unsigned formats
  DDPF_BUMPDUDV        = $00080000;    // used by signed formats

  { Constants used by TDDSCaps.Caps1.}
  DDSCAPS_COMPLEX      = $00000008;
  DDSCAPS_TEXTURE      = $00001000;
  DDSCAPS_MIPMAP       = $00400000;

  { Constants used by TDDSCaps.Caps2.}
  DDSCAPS2_CUBEMAP     = $00000200;
  DDSCAPS2_POSITIVEX   = $00000400;
  DDSCAPS2_NEGATIVEX   = $00000800;
  DDSCAPS2_POSITIVEY   = $00001000;
  DDSCAPS2_NEGATIVEY   = $00002000;
  DDSCAPS2_POSITIVEZ   = $00004000;
  DDSCAPS2_NEGATIVEZ   = $00008000;
  DDSCAPS2_VOLUME      = $00200000;

  { Flags for TDDSurfaceDesc2.Flags used when saving DDS file.}
  DDS_SAVE_FLAGS = DDSD_CAPS or DDSD_PIXELFORMAT or DDSD_WIDTH or
    DDSD_HEIGHT or DDSD_LINEARSIZE;

type
  { Stores the pixel format information.}
  TDDPixelFormat = packed record
    Size: UInt32;       // Size of the structure = 32 bytes
    Flags: UInt32;      // Flags to indicate valid fields
    FourCC: UInt32;     // Four-char code for compressed textures (DXT)
    BitCount: UInt32;   // Bits per pixel if uncomp. usually 16,24 or 32
    RedMask: UInt32;    // Bit mask for the Red component
    GreenMask: UInt32;  // Bit mask for the Green component
    BlueMask: UInt32;   // Bit mask for the Blue component
    AlphaMask: UInt32;  // Bit mask for the Alpha component
  end;

  { Specifies capabilities of surface.}
  TDDSCaps = packed record
    Caps1: UInt32;      // Should always include DDSCAPS_TEXTURE
    Caps2: UInt32;      // For cubic environment maps
    Reserved: array[0..1] of UInt32; // Reserved
  end;

  { Record describing DDS file contents.}
  TDDSurfaceDesc2 = packed record
    Size: UInt32;       // Size of the structure = 124 Bytes
    Flags: UInt32;      // Flags to indicate valid fields
    Height: UInt32;     // Height of the main image in pixels
    Width: UInt32;      // Width of the main image in pixels
    PitchOrLinearSize: UInt32; // For uncomp formats number of bytes per
                               // scanline. For comp it is the size in
                               // bytes of the main image
    Depth: UInt32;      // Only for volume text depth of the volume
    MipMaps: Int32;     // Total number of levels in the mipmap chain
    Reserved1: array[0..10] of UInt32; // Reserved
    PixelFormat: TDDPixelFormat; // Format of the pixel data
    Caps: TDDSCaps;       // Capabilities
    Reserved2: UInt32;  // Reserved
  end;

  { DDS file header.}
  TDDSFileHeader = packed record
    Magic: UInt32;       // File format magic
    Desc: TDDSurfaceDesc2; // Surface description
  end;

  { Resource types for D3D 10+ }
  TD3D10ResourceDimension = (
    D3D10_RESOURCE_DIMENSION_UNKNOWN   = 0,
    D3D10_RESOURCE_DIMENSION_BUFFER    = 1,
    D3D10_RESOURCE_DIMENSION_TEXTURE1D = 2,
    D3D10_RESOURCE_DIMENSION_TEXTURE2D = 3,
    D3D10_RESOURCE_DIMENSION_TEXTURE3D = 4
  );

  { Texture formats for D3D 10+ }
  TDXGIFormat = (
    DXGI_FORMAT_UNKNOWN                      = 0,
    DXGI_FORMAT_R32G32B32A32_TYPELESS        = 1,
    DXGI_FORMAT_R32G32B32A32_FLOAT           = 2,
    DXGI_FORMAT_R32G32B32A32_UINT            = 3,
    DXGI_FORMAT_R32G32B32A32_SINT            = 4,
    DXGI_FORMAT_R32G32B32_TYPELESS           = 5,
    DXGI_FORMAT_R32G32B32_FLOAT              = 6,
    DXGI_FORMAT_R32G32B32_UINT               = 7,
    DXGI_FORMAT_R32G32B32_SINT               = 8,
    DXGI_FORMAT_R16G16B16A16_TYPELESS        = 9,
    DXGI_FORMAT_R16G16B16A16_FLOAT           = 10,
    DXGI_FORMAT_R16G16B16A16_UNORM           = 11,
    DXGI_FORMAT_R16G16B16A16_UINT            = 12,
    DXGI_FORMAT_R16G16B16A16_SNORM           = 13,
    DXGI_FORMAT_R16G16B16A16_SINT            = 14,
    DXGI_FORMAT_R32G32_TYPELESS              = 15,
    DXGI_FORMAT_R32G32_FLOAT                 = 16,
    DXGI_FORMAT_R32G32_UINT                  = 17,
    DXGI_FORMAT_R32G32_SINT                  = 18,
    DXGI_FORMAT_R32G8X24_TYPELESS            = 19,
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT         = 20,
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS     = 21,
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT      = 22,
    DXGI_FORMAT_R10G10B10A2_TYPELESS         = 23,
    DXGI_FORMAT_R10G10B10A2_UNORM            = 24,
    DXGI_FORMAT_R10G10B10A2_UINT             = 25,
    DXGI_FORMAT_R11G11B10_FLOAT              = 26,
    DXGI_FORMAT_R8G8B8A8_TYPELESS            = 27,
    DXGI_FORMAT_R8G8B8A8_UNORM               = 28,
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB          = 29,
    DXGI_FORMAT_R8G8B8A8_UINT                = 30,
    DXGI_FORMAT_R8G8B8A8_SNORM               = 31,
    DXGI_FORMAT_R8G8B8A8_SINT                = 32,
    DXGI_FORMAT_R16G16_TYPELESS              = 33,
    DXGI_FORMAT_R16G16_FLOAT                 = 34,
    DXGI_FORMAT_R16G16_UNORM                 = 35,
    DXGI_FORMAT_R16G16_UINT                  = 36,
    DXGI_FORMAT_R16G16_SNORM                 = 37,
    DXGI_FORMAT_R16G16_SINT                  = 38,
    DXGI_FORMAT_R32_TYPELESS                 = 39,
    DXGI_FORMAT_D32_FLOAT                    = 40,
    DXGI_FORMAT_R32_FLOAT                    = 41,
    DXGI_FORMAT_R32_UINT                     = 42,
    DXGI_FORMAT_R32_SINT                     = 43,
    DXGI_FORMAT_R24G8_TYPELESS               = 44,
    DXGI_FORMAT_D24_UNORM_S8_UINT            = 45,
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS        = 46,
    DXGI_FORMAT_X24_TYPELESS_G8_UINT         = 47,
    DXGI_FORMAT_R8G8_TYPELESS                = 48,
    DXGI_FORMAT_R8G8_UNORM                   = 49,
    DXGI_FORMAT_R8G8_UINT                    = 50,
    DXGI_FORMAT_R8G8_SNORM                   = 51,
    DXGI_FORMAT_R8G8_SINT                    = 52,
    DXGI_FORMAT_R16_TYPELESS                 = 53,
    DXGI_FORMAT_R16_FLOAT                    = 54,
    DXGI_FORMAT_D16_UNORM                    = 55,
    DXGI_FORMAT_R16_UNORM                    = 56,
    DXGI_FORMAT_R16_UINT                     = 57,
    DXGI_FORMAT_R16_SNORM                    = 58,
    DXGI_FORMAT_R16_SINT                     = 59,
    DXGI_FORMAT_R8_TYPELESS                  = 60,
    DXGI_FORMAT_R8_UNORM                     = 61,
    DXGI_FORMAT_R8_UINT                      = 62,
    DXGI_FORMAT_R8_SNORM                     = 63,
    DXGI_FORMAT_R8_SINT                      = 64,
    DXGI_FORMAT_A8_UNORM                     = 65,
    DXGI_FORMAT_R1_UNORM                     = 66,
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP           = 67,
    DXGI_FORMAT_R8G8_B8G8_UNORM              = 68,
    DXGI_FORMAT_G8R8_G8B8_UNORM              = 69,
    DXGI_FORMAT_BC1_TYPELESS                 = 70,
    DXGI_FORMAT_BC1_UNORM                    = 71,
    DXGI_FORMAT_BC1_UNORM_SRGB               = 72,
    DXGI_FORMAT_BC2_TYPELESS                 = 73,
    DXGI_FORMAT_BC2_UNORM                    = 74,
    DXGI_FORMAT_BC2_UNORM_SRGB               = 75,
    DXGI_FORMAT_BC3_TYPELESS                 = 76,
    DXGI_FORMAT_BC3_UNORM                    = 77,
    DXGI_FORMAT_BC3_UNORM_SRGB               = 78,
    DXGI_FORMAT_BC4_TYPELESS                 = 79,
    DXGI_FORMAT_BC4_UNORM                    = 80,
    DXGI_FORMAT_BC4_SNORM                    = 81,
    DXGI_FORMAT_BC5_TYPELESS                 = 82,
    DXGI_FORMAT_BC5_UNORM                    = 83,
    DXGI_FORMAT_BC5_SNORM                    = 84,
    DXGI_FORMAT_B5G6R5_UNORM                 = 85,
    DXGI_FORMAT_B5G5R5A1_UNORM               = 86,
    DXGI_FORMAT_B8G8R8A8_UNORM               = 87,
    DXGI_FORMAT_B8G8R8X8_UNORM               = 88,
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM   = 89,
    DXGI_FORMAT_B8G8R8A8_TYPELESS            = 90,
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB          = 91,
    DXGI_FORMAT_B8G8R8X8_TYPELESS            = 92,
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB          = 93,
    DXGI_FORMAT_BC6H_TYPELESS                = 94,
    DXGI_FORMAT_BC6H_UF16                    = 95,
    DXGI_FORMAT_BC6H_SF16                    = 96,
    DXGI_FORMAT_BC7_TYPELESS                 = 97,
    DXGI_FORMAT_BC7_UNORM                    = 98,
    DXGI_FORMAT_BC7_UNORM_SRGB               = 99,
    DXGI_FORMAT_AYUV                         = 100,
    DXGI_FORMAT_Y410                         = 101,
    DXGI_FORMAT_Y416                         = 102,
    DXGI_FORMAT_NV12                         = 103,
    DXGI_FORMAT_P010                         = 104,
    DXGI_FORMAT_P016                         = 105,
    DXGI_FORMAT_420_OPAQUE                   = 106,
    DXGI_FORMAT_YUY2                         = 107,
    DXGI_FORMAT_Y210                         = 108,
    DXGI_FORMAT_Y216                         = 109,
    DXGI_FORMAT_NV11                         = 110,
    DXGI_FORMAT_AI44                         = 111,
    DXGI_FORMAT_IA44                         = 112,
    DXGI_FORMAT_P8                           = 113,
    DXGI_FORMAT_A8P8                         = 114,
    DXGI_FORMAT_B4G4R4A4_UNORM               = 115
  );

  { DX10 extension header for DDS file format }
  TDX10Header = packed record
    DXGIFormat: TDXGIFormat;
    ResourceDimension: TD3D10ResourceDimension;
    MiscFlags: UInt32;
    ArraySize: UInt32;
    Reserved: UInt32;
  end;

{ TDDSFileFormat class implementation }

procedure TDDSFileFormat.Define;
begin
  inherited;
  FName := SDDSFormatName;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FSupportedFormats := DDSSupportedFormats;

  FSaveCubeMap := False;
  FSaveVolume := False;
  FSaveMipMapCount := 1;
  FSaveDepth := 1;

  AddMasks(SDDSMasks);

  RegisterOption(ImagingDDSLoadedCubeMap, @FLoadedCubeMap);
  RegisterOption(ImagingDDSLoadedVolume, @FLoadedVolume);
  RegisterOption(ImagingDDSLoadedMipMapCount, @FLoadedMipMapCount);
  RegisterOption(ImagingDDSLoadedDepth, @FLoadedDepth);
  RegisterOption(ImagingDDSSaveCubeMap, @FSaveCubeMap);
  RegisterOption(ImagingDDSSaveVolume, @FSaveVolume);
  RegisterOption(ImagingDDSSaveMipMapCount, @FSaveMipMapCount);
  RegisterOption(ImagingDDSSaveDepth, @FSaveDepth);
end;

procedure TDDSFileFormat.CheckOptionsValidity;
begin
  if FSaveCubeMap then
    FSaveVolume := False;
  if FSaveVolume then
    FSaveCubeMap := False;
  if FSaveDepth < 1 then
    FSaveDepth := 1;
  if FSaveMipMapCount < 1 then
    FSaveMipMapCount := 1;
end;

procedure TDDSFileFormat.ComputeSubDimensions(Idx, Width, Height, MipMaps, Depth: LongInt;
  IsCubeMap, IsVolume: Boolean; var CurWidth, CurHeight: LongInt);
var
  I, Last, Shift: LongInt;
begin
  CurWidth := Width;
  CurHeight := Height;
  if MipMaps > 1 then
  begin
    if not IsVolume then
    begin
      if IsCubeMap then
      begin
        // Cube maps are stored like this
        // Face 0 mipmap 0
        // Face 0 mipmap 1
        // ...
        // Face 1 mipmap 0
        // Face 1 mipmap 1
        // ...

        // Modify index so later in for loop we iterate less times
        Idx := Idx - ((Idx div MipMaps) * MipMaps);
      end;
      for I := 0 to Idx - 1 do
      begin
        CurWidth := ClampInt(CurWidth shr 1, 1, CurWidth);
        CurHeight := ClampInt(CurHeight shr 1, 1, CurHeight);
      end;
    end
    else
    begin
      // Volume textures are stored in DDS files like this:
      // Slice 0 mipmap 0
      // Slice 1 mipmap 0
      // Slice 2 mipmap 0
      // Slice 3 mipmap 0
      // Slice 0 mipmap 1
      // Slice 1 mipmap 1
      // Slice 0 mipmap 2
      // Slice 0 mipmap 3 ...
      Shift := 0;
      Last := Depth;
      while Idx > Last - 1 do
      begin
        CurWidth := ClampInt(CurWidth shr 1, 1, CurWidth);
        CurHeight := ClampInt(CurHeight shr 1, 1, CurHeight);
        if (CurWidth = 1) and (CurHeight = 1) then
          Break;
        Inc(Shift);
        Inc(Last, ClampInt(Depth shr Shift, 1, Depth));
      end;
    end;
  end;
end;

function TDDSFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TDDSFileHeader;
  HdrDX10: TDX10Header;
  SrcFormat: TImageFormat;
  FmtInfo: TImageFormatInfo;
  NeedsSwapChannels: Boolean;
  CurrentWidth, CurrentHeight, ImageCount, LoadSize, I,
    PitchOrLinear, MainImageLinearSize: LongInt;
  Data: PByte;
  UseAsPitch: Boolean;
  UseAsLinear: Boolean;

  function MasksEqual(const DDPF: TDDPixelFormat; PF: PPixelFormatInfo): Boolean;
  begin
    Result := (DDPF.AlphaMask = PF.ABitMask) and
      (DDPF.RedMask = PF.RBitMask) and (DDPF.GreenMask = PF.GBitMask) and
      (DDPF.BlueMask = PF.BBitMask);
  end;

  function FindFourCCFormat(FourCC: UInt32): TImageFormat;
  begin
    // Handle FourCC and large ARGB formats
    case FourCC of
      D3DFMT_A16B16G16R16: Result := ifA16B16G16R16;
      D3DFMT_R32F: Result := ifR32F;
      D3DFMT_A32B32G32R32F: Result := ifA32B32G32R32F;
      D3DFMT_R16F: Result := ifR16F;
      D3DFMT_A16B16G16R16F: Result := ifA16B16G16R16F;
      FOURCC_DXT1: Result := ifDXT1;
      FOURCC_DXT3: Result := ifDXT3;
      FOURCC_DXT5: Result := ifDXT5;
      FOURCC_ATI1: Result := ifATI1N;
      FOURCC_ATI2: Result := ifATI2N;
    else
      Result := ifUnknown;
    end;
  end;

  function FindDX10Format(DXGIFormat: TDXGIFormat; var NeedsSwapChannels: Boolean): TImageFormat;
  begin
    Result := ifUnknown;
    NeedsSwapChannels := False;

    case DXGIFormat of
      DXGI_FORMAT_UNKNOWN: ;
      DXGI_FORMAT_R32G32B32A32_TYPELESS, DXGI_FORMAT_R32G32B32A32_FLOAT:
        Result := ifA32B32G32R32F;
      DXGI_FORMAT_R32G32B32A32_UINT: ;
      DXGI_FORMAT_R32G32B32A32_SINT: ;
      DXGI_FORMAT_R32G32B32_TYPELESS, DXGI_FORMAT_R32G32B32_FLOAT:
        Result := ifB32G32R32F;
      DXGI_FORMAT_R32G32B32_UINT: ;
      DXGI_FORMAT_R32G32B32_SINT: ;
      DXGI_FORMAT_R16G16B16A16_FLOAT:
        Result := ifA16B16G16R16F;
      DXGI_FORMAT_R16G16B16A16_TYPELESS, DXGI_FORMAT_R16G16B16A16_UNORM,
      DXGI_FORMAT_R16G16B16A16_UINT, DXGI_FORMAT_R16G16B16A16_SNORM,
      DXGI_FORMAT_R16G16B16A16_SINT:
        Result := ifA16B16G16R16;
      DXGI_FORMAT_R32G32_TYPELESS: ;
      DXGI_FORMAT_R32G32_FLOAT: ;
      DXGI_FORMAT_R32G32_UINT: ;
      DXGI_FORMAT_R32G32_SINT: ;
      DXGI_FORMAT_R32G8X24_TYPELESS: ;
      DXGI_FORMAT_D32_FLOAT_S8X24_UINT: ;
      DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS: ;
      DXGI_FORMAT_X32_TYPELESS_G8X24_UINT: ;
      DXGI_FORMAT_R10G10B10A2_TYPELESS: ;
      DXGI_FORMAT_R10G10B10A2_UNORM: ;
      DXGI_FORMAT_R10G10B10A2_UINT: ;
      DXGI_FORMAT_R11G11B10_FLOAT: ;
      DXGI_FORMAT_R8G8B8A8_TYPELESS, DXGI_FORMAT_R8G8B8A8_UNORM,
      DXGI_FORMAT_R8G8B8A8_UINT, DXGI_FORMAT_R8G8B8A8_SNORM,DXGI_FORMAT_R8G8B8A8_SINT,
      DXGI_FORMAT_R8G8B8A8_UNORM_SRGB:
        begin
          Result := ifA8R8G8B8;
          NeedsSwapChannels := True;
        end;
      DXGI_FORMAT_R16G16_TYPELESS: ;
      DXGI_FORMAT_R16G16_FLOAT: ;
      DXGI_FORMAT_R16G16_UNORM: ;
      DXGI_FORMAT_R16G16_UINT: ;
      DXGI_FORMAT_R16G16_SNORM: ;
      DXGI_FORMAT_R16G16_SINT: ;
      DXGI_FORMAT_R32_TYPELESS, DXGI_FORMAT_R32_UINT, DXGI_FORMAT_R32_SINT:
        Result := ifGray32;
      DXGI_FORMAT_D32_FLOAT, DXGI_FORMAT_R32_FLOAT:
        Result := ifR32F;
      DXGI_FORMAT_R24G8_TYPELESS: ;
      DXGI_FORMAT_D24_UNORM_S8_UINT: ;
      DXGI_FORMAT_R24_UNORM_X8_TYPELESS: ;
      DXGI_FORMAT_X24_TYPELESS_G8_UINT: ;
      DXGI_FORMAT_R8G8_TYPELESS, DXGI_FORMAT_R8G8_UNORM, DXGI_FORMAT_R8G8_UINT,
      DXGI_FORMAT_R8G8_SNORM, DXGI_FORMAT_R8G8_SINT:
        Result := ifA8Gray8;
      DXGI_FORMAT_R16_TYPELESS, DXGI_FORMAT_D16_UNORM, DXGI_FORMAT_R16_UNORM,
      DXGI_FORMAT_R16_UINT, DXGI_FORMAT_R16_SNORM, DXGI_FORMAT_R16_SINT:
        Result := ifGray16;
      DXGI_FORMAT_R16_FLOAT:
        Result := ifR16F;
      DXGI_FORMAT_R8_TYPELESS, DXGI_FORMAT_R8_UNORM, DXGI_FORMAT_R8_UINT,
      DXGI_FORMAT_R8_SNORM, DXGI_FORMAT_R8_SINT, DXGI_FORMAT_A8_UNORM:
        Result := ifGray8;
      DXGI_FORMAT_R1_UNORM: ;
      DXGI_FORMAT_R9G9B9E5_SHAREDEXP: ;
      DXGI_FORMAT_R8G8_B8G8_UNORM: ;
      DXGI_FORMAT_G8R8_G8B8_UNORM: ;
      DXGI_FORMAT_BC1_TYPELESS, DXGI_FORMAT_BC1_UNORM, DXGI_FORMAT_BC1_UNORM_SRGB:
        Result := ifDXT1;
      DXGI_FORMAT_BC2_TYPELESS, DXGI_FORMAT_BC2_UNORM, DXGI_FORMAT_BC2_UNORM_SRGB:
        Result := ifDXT3;
      DXGI_FORMAT_BC3_TYPELESS, DXGI_FORMAT_BC3_UNORM, DXGI_FORMAT_BC3_UNORM_SRGB:
        Result := ifDXT5;
      DXGI_FORMAT_BC4_TYPELESS, DXGI_FORMAT_BC4_UNORM, DXGI_FORMAT_BC4_SNORM:
        Result := ifATI1N;
      DXGI_FORMAT_BC5_TYPELESS, DXGI_FORMAT_BC5_UNORM, DXGI_FORMAT_BC5_SNORM:
        Result := ifATI2N;
      DXGI_FORMAT_B5G6R5_UNORM:
        Result := ifR5G6B5;
      DXGI_FORMAT_B5G5R5A1_UNORM:
        Result := ifA1R5G5B5;
      DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_FORMAT_B8G8R8A8_TYPELESS:
        Result := ifA8R8G8B8;
      DXGI_FORMAT_B8G8R8X8_UNORM, DXGI_FORMAT_B8G8R8X8_TYPELESS:
        Result := ifX8R8G8B8;
      DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM: ;
      DXGI_FORMAT_B8G8R8A8_UNORM_SRGB: ;
      DXGI_FORMAT_B8G8R8X8_UNORM_SRGB: ;
      DXGI_FORMAT_BC6H_TYPELESS: ;
      DXGI_FORMAT_BC6H_UF16: ;
      DXGI_FORMAT_BC6H_SF16: ;
      DXGI_FORMAT_BC7_TYPELESS: ;
      DXGI_FORMAT_BC7_UNORM: ;
      DXGI_FORMAT_BC7_UNORM_SRGB: ;
      DXGI_FORMAT_P8: ;
      DXGI_FORMAT_A8P8: ;
      DXGI_FORMAT_B4G4R4A4_UNORM:
        Result := ifA4R4G4B4;
    end;
  end;

begin
  Result := False;
  ImageCount := 1;
  FLoadedMipMapCount := 1;
  FLoadedDepth := 1;
  FLoadedVolume := False;
  FLoadedCubeMap := False;
  ZeroMemory(@HdrDX10, SizeOf(HdrDX10));

  with GetIO, Hdr, Hdr.Desc.PixelFormat do
  begin
    Read(Handle, @Hdr, SizeOf(Hdr));

    SrcFormat := ifUnknown;
    NeedsSwapChannels := False;

    // Get image data format
    if (Flags and DDPF_FOURCC) = DDPF_FOURCC then
    begin
      if FourCC = FOURCC_DX10 then
      begin
        Read(Handle, @HdrDX10, SizeOf(HdrDX10));
        SrcFormat := FindDX10Format(HdrDX10.DXGIFormat, NeedsSwapChannels);
        FMetadata.SetMetaItem(SMetaDdsDxgiFormat, HdrDX10.DXGIFormat);
        FMetadata.SetMetaItem(SMetaDdsArraySize, HdrDX10.ArraySize);
      end
      else
        SrcFormat := FindFourCCFormat(FourCC);
    end
    else if (Flags and DDPF_RGB) = DDPF_RGB then
    begin
      // Handle RGB formats
      if (Flags and DDPF_ALPHAPIXELS) = DDPF_ALPHAPIXELS then
      begin
        // Handle RGB with alpha formats
        case BitCount of
          16:
            begin
              if MasksEqual(Desc.PixelFormat, GetFormatInfo(ifA4R4G4B4).PixelFormat) then
                SrcFormat := ifA4R4G4B4;
              if MasksEqual(Desc.PixelFormat, GetFormatInfo(ifA1R5G5B5).PixelFormat) then
                SrcFormat := ifA1R5G5B5;
            end;
          32:
            begin
              SrcFormat := ifA8R8G8B8;
              if BlueMask = $00FF0000 then
                NeedsSwapChannels := True;
            end;
        end;
      end
      else
      begin
        // Handle RGB without alpha formats
        case BitCount of
          8:
            if MasksEqual(Desc.PixelFormat,
              GetFormatInfo(ifR3G3B2).PixelFormat) then
              SrcFormat := ifR3G3B2;
          16:
            begin
              if MasksEqual(Desc.PixelFormat,
                GetFormatInfo(ifX4R4G4B4).PixelFormat) then
                SrcFormat := ifX4R4G4B4;
              if MasksEqual(Desc.PixelFormat,
                GetFormatInfo(ifX1R5G5B5).PixelFormat) then
                SrcFormat := ifX1R5G5B5;
              if MasksEqual(Desc.PixelFormat,
                GetFormatInfo(ifR5G6B5).PixelFormat) then
                SrcFormat := ifR5G6B5;
            end;
          24: SrcFormat := ifR8G8B8;
          32:
            begin
              SrcFormat := ifX8R8G8B8;
              if BlueMask = $00FF0000 then
                NeedsSwapChannels := True;
            end;
        end;
      end;
    end
    else if (Flags and DDPF_LUMINANCE) = DDPF_LUMINANCE then
    begin
      // Handle luminance formats
      if (Flags and DDPF_ALPHAPIXELS) = DDPF_ALPHAPIXELS then
      begin
        // Handle luminance with alpha formats
        if BitCount = 16 then
          SrcFormat := ifA8Gray8;
      end
      else
      begin
        // Handle luminance without alpha formats
        case BitCount of
          8: SrcFormat := ifGray8;
          16: SrcFormat := ifGray16;
        end;
      end;
    end
    else if (Flags and DDPF_BUMPLUMINANCE) = DDPF_BUMPLUMINANCE then
    begin
      // Handle mixed bump-luminance formats like D3DFMT_X8L8V8U8
      case BitCount of
        32:
          if BlueMask = $00FF0000 then
          begin
            SrcFormat := ifX8R8G8B8; // D3DFMT_X8L8V8U8
            NeedsSwapChannels := True;
          end;
      end;
    end
    else if (Flags and DDPF_BUMPDUDV) = DDPF_BUMPDUDV then
    begin
      // Handle bumpmap formats like D3DFMT_Q8W8V8U8
      case BitCount of
        16: SrcFormat := ifA8Gray8; // D3DFMT_V8U8
        32:
          if AlphaMask = $FF000000 then
          begin
            SrcFormat := ifA8R8G8B8; // D3DFMT_Q8W8V8U8
            NeedsSwapChannels := True;
          end;
        64: SrcFormat := ifA16B16G16R16; // D3DFMT_Q16W16V16U16
      end;
    end;

    // If DDS format is not supported we will exit
    if SrcFormat = ifUnknown then
      Exit;

    // File contains mipmaps for each subimage.
    { Some DDS writers ignore setting proper Caps and Flags so
      this check is not usable:
    if ((Desc.Caps.Caps1 and DDSCAPS_MIPMAP) = DDSCAPS_MIPMAP) and
      ((Desc.Flags and DDSD_MIPMAPCOUNT) = DDSD_MIPMAPCOUNT) then}
    if Desc.MipMaps > 1 then
    begin
      FLoadedMipMapCount := Desc.MipMaps;
      FMetadata.SetMetaItem(SMetaDdsMipMapCount, Desc.MipMaps);
      ImageCount := Desc.MipMaps;
    end;

    // File stores volume texture
    if ((Desc.Caps.Caps2 and DDSCAPS2_VOLUME) = DDSCAPS2_VOLUME) and
      ((Desc.Flags and DDSD_DEPTH) = DDSD_DEPTH) then
    begin
      FLoadedVolume := True;
      FLoadedDepth := Desc.Depth;
      ImageCount := GetVolumeLevelCount(Desc.Depth, ImageCount);
    end;

    // File stores cube texture
    if (Desc.Caps.Caps2 and DDSCAPS2_CUBEMAP) = DDSCAPS2_CUBEMAP then
    begin
      FLoadedCubeMap := True;
      I := 0;
      if (Desc.Caps.Caps2 and DDSCAPS2_POSITIVEX) = DDSCAPS2_POSITIVEX then Inc(I);
      if (Desc.Caps.Caps2 and DDSCAPS2_POSITIVEY) = DDSCAPS2_POSITIVEY then Inc(I);
      if (Desc.Caps.Caps2 and DDSCAPS2_POSITIVEZ) = DDSCAPS2_POSITIVEZ then Inc(I);
      if (Desc.Caps.Caps2 and DDSCAPS2_NEGATIVEX) = DDSCAPS2_NEGATIVEX then Inc(I);
      if (Desc.Caps.Caps2 and DDSCAPS2_NEGATIVEY) = DDSCAPS2_NEGATIVEY then Inc(I);
      if (Desc.Caps.Caps2 and DDSCAPS2_NEGATIVEZ) = DDSCAPS2_NEGATIVEZ then Inc(I);
      FLoadedDepth := I;
      ImageCount := ImageCount * I;
    end;

    // Allocate and load all images in file
    FmtInfo := GetFormatInfo(SrcFormat);
    SetLength(Images, ImageCount);

    // Compute the pitch or get if from file if present
    UseAsPitch := (Desc.Flags and DDSD_PITCH) = DDSD_PITCH;
    UseAsLinear := (Desc.Flags and DDSD_LINEARSIZE) = DDSD_LINEARSIZE;
    // Use linear as default if none is set
    if not UseAsPitch and not UseAsLinear then
      UseAsLinear := True;
    // Main image pitch or linear size
    PitchOrLinear := Desc.PitchOrLinearSize;

    // Check: some writers just write garbage to pitch/linear size fields and flags
    MainImageLinearSize := FmtInfo.GetPixelsSize(SrcFormat, Desc.Width, Desc.Height);
    if UseAsLinear and ((PitchOrLinear < MainImageLinearSize) or
      (PitchOrLinear * Integer(Desc.Height) = MainImageLinearSize)) then
    begin
      // Explicitly set linear size
      PitchOrLinear := MainImageLinearSize;
    end;

    for I := 0 to ImageCount - 1 do
    begin
      // Compute dimensions of surrent subimage based on texture type and
      // number of mipmaps
      ComputeSubDimensions(I, Desc.Width, Desc.Height, Desc.MipMaps, Desc.Depth,
        FLoadedCubeMap, FLoadedVolume, CurrentWidth, CurrentHeight);
      NewImage(CurrentWidth, CurrentHeight, SrcFormat, Images[I]);

      if (I > 0) or (PitchOrLinear = 0) then
      begin
        // Compute pitch or linear size for mipmap levels, or even for main image
        // since some formats do not fill pitch nor size
        if UseAsLinear then
          PitchOrLinear := FmtInfo.GetPixelsSize(SrcFormat, CurrentWidth, CurrentHeight)
        else
          PitchOrLinear := (CurrentWidth * FmtInfo.BytesPerPixel + 3) div 4 * 4; // must be DWORD aligned
      end;

      if UseAsLinear then
        LoadSize := PitchOrLinear
      else
        LoadSize := CurrentHeight * PitchOrLinear;

      if UseAsLinear or (LoadSize = Images[I].Size) then
      begin
        // If DDS does not use Pitch we can simply copy data
        Read(Handle, Images[I].Bits, LoadSize)
      end
      else
      begin
        // If DDS uses Pitch we must load aligned scanlines
        // and then remove padding
        GetMem(Data, LoadSize);
        try
          Read(Handle, Data, LoadSize);
          RemovePadBytes(Data, Images[I].Bits, CurrentWidth, CurrentHeight,
            FmtInfo.BytesPerPixel, PitchOrLinear);
       finally
          FreeMem(Data);
        end;
      end;

      if NeedsSwapChannels then
        SwapChannels(Images[I], ChannelRed, ChannelBlue);
    end;
    Result := True;
  end;
end;

function TDDSFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Hdr: TDDSFileHeader;
  MainImage, ImageToSave: TImageData;
  I, MainIdx, Len, ImageCount: LongInt;
  J: UInt32;
  FmtInfo: TImageFormatInfo;
  MustBeFreed: Boolean;
  Is2DTexture, IsCubeMap, IsVolume: Boolean;
  MipMapCount, CurrentWidth, CurrentHeight: LongInt;
  NeedsResize: Boolean;
  NeedsConvert: Boolean;
begin
  Result := False;
  FillChar(Hdr, Sizeof(Hdr), 0);

  MainIdx := FFirstIdx;
  Len := FLastIdx - MainIdx + 1;
  // Some DDS saving rules:
  //   2D textures: Len is used as mipmap count (FSaveMipMapCount not used!).
  //   Cube maps:   FSaveDepth * FSaveMipMapCount images are used, if Len is
  //                smaller than this file is saved as regular 2D texture.
  //   Volume maps: GetVolumeLevelCount(FSaveDepth, FSaveMipMapCount) images are
  //                used, if Len is smaller than this file is
  //                saved as regular 2D texture.

  IsCubeMap := FSaveCubeMap;
  IsVolume := FSaveVolume;
  MipMapCount := FSaveMipMapCount;

  if IsCubeMap then
  begin
    // Check if we have enough images on Input to save cube map
    if Len < FSaveDepth * FSaveMipMapCount then
      IsCubeMap := False;
  end
  else if IsVolume then
  begin
    // Check if we have enough images on Input to save volume texture
    if Len < GetVolumeLevelCount(FSaveDepth, FSaveMipMapCount) then
      IsVolume := False;
  end;

  Is2DTexture := not IsCubeMap and not IsVolume;
  if Is2DTexture then
  begin
    // Get number of mipmaps used with 2D texture
    MipMapCount := Min(Len, GetNumMipMapLevels(Images[MainIdx].Width, Images[MainIdx].Height));
  end;

  // we create compatible main image and fill headers
  if MakeCompatible(Images[MainIdx], MainImage, MustBeFreed) then
  with GetIO, MainImage, Hdr do
  try
    FmtInfo := GetFormatInfo(Format);
    Magic := DDSMagic;
    Desc.Size := SizeOf(Desc);
    Desc.Width := Width;
    Desc.Height := Height;
    Desc.Flags := DDS_SAVE_FLAGS;
    Desc.Caps.Caps1 := DDSCAPS_TEXTURE;
    Desc.PixelFormat.Size := SizeOf(Desc.PixelFormat);
    Desc.PitchOrLinearSize := MainImage.Size;
    ImageCount := MipMapCount;

    if MipMapCount > 1 then
    begin
      // Set proper flags if we have some mipmaps to be saved
      Desc.Flags := Desc.Flags or DDSD_MIPMAPCOUNT;
      Desc.Caps.Caps1 := Desc.Caps.Caps1 or DDSCAPS_MIPMAP or DDSCAPS_COMPLEX;
      Desc.MipMaps := MipMapCount;
    end;

    if IsCubeMap then
    begin
      // Set proper cube map flags - number of stored faces is taken
      // from FSaveDepth
      Desc.Caps.Caps1 := Desc.Caps.Caps1 or DDSCAPS_COMPLEX;
      Desc.Caps.Caps2 := Desc.Caps.Caps2 or DDSCAPS2_CUBEMAP;
      J := DDSCAPS2_POSITIVEX;
      for I := 0 to FSaveDepth - 1 do
      begin
        Desc.Caps.Caps2 := Desc.Caps.Caps2 or J;
        J := J shl 1;
      end;
      ImageCount := FSaveDepth * FSaveMipMapCount;
    end
    else if IsVolume then
    begin
      // Set proper flags for volume texture
      Desc.Flags := Desc.Flags or DDSD_DEPTH;
      Desc.Caps.Caps1 := Desc.Caps.Caps1 or DDSCAPS_COMPLEX;
      Desc.Caps.Caps2 := Desc.Caps.Caps2 or DDSCAPS2_VOLUME;
      Desc.Depth := FSaveDepth;
      ImageCount := GetVolumeLevelCount(FSaveDepth, FSaveMipMapCount);
    end;

    // Now we set DDS pixel format for main image
    if FmtInfo.IsSpecial or FmtInfo.IsFloatingPoint or
      (FmtInfo.BytesPerPixel > 4) then
    begin
      Desc.PixelFormat.Flags := DDPF_FOURCC;
      case Format of
        ifA16B16G16R16:  Desc.PixelFormat.FourCC := D3DFMT_A16B16G16R16;
        ifR32F:          Desc.PixelFormat.FourCC := D3DFMT_R32F;
        ifA32B32G32R32F: Desc.PixelFormat.FourCC := D3DFMT_A32B32G32R32F;
        ifR16F:          Desc.PixelFormat.FourCC := D3DFMT_R16F;
        ifA16B16G16R16F: Desc.PixelFormat.FourCC := D3DFMT_A16B16G16R16F;
        ifDXT1:          Desc.PixelFormat.FourCC := FOURCC_DXT1;
        ifDXT3:          Desc.PixelFormat.FourCC := FOURCC_DXT3;
        ifDXT5:          Desc.PixelFormat.FourCC := FOURCC_DXT5;
        ifATI1N:         Desc.PixelFormat.FourCC := FOURCC_ATI1;
        ifATI2N:         Desc.PixelFormat.FourCC := FOURCC_ATI2;
      end;
    end
    else if FmtInfo.HasGrayChannel then
    begin
      Desc.PixelFormat.Flags := DDPF_LUMINANCE;
      Desc.PixelFormat.BitCount := FmtInfo.BytesPerPixel * 8;
      case Format of
        ifGray8:  Desc.PixelFormat.RedMask := 255;
        ifGray16: Desc.PixelFormat.RedMask := 65535;
        ifA8Gray8:
          begin
            Desc.PixelFormat.Flags := Desc.PixelFormat.Flags or DDPF_ALPHAPIXELS;
            Desc.PixelFormat.RedMask := 255;
            Desc.PixelFormat.AlphaMask := 65280;
          end;
      end;
    end
    else
    begin
      Desc.PixelFormat.Flags := DDPF_RGB;
      Desc.PixelFormat.BitCount := FmtInfo.BytesPerPixel * 8;
      if FmtInfo.HasAlphaChannel then
      begin
        Desc.PixelFormat.Flags := Desc.PixelFormat.Flags or DDPF_ALPHAPIXELS;
        Desc.PixelFormat.AlphaMask := $FF000000;
      end;
      if FmtInfo.BytesPerPixel > 2 then
      begin
        Desc.PixelFormat.RedMask :=   $00FF0000;
        Desc.PixelFormat.GreenMask := $0000FF00;
        Desc.PixelFormat.BlueMask :=  $000000FF;
      end
      else
      begin
        Desc.PixelFormat.AlphaMask := FmtInfo.PixelFormat.ABitMask;
        Desc.PixelFormat.RedMask := FmtInfo.PixelFormat.RBitMask;
        Desc.PixelFormat.GreenMask := FmtInfo.PixelFormat.GBitMask;
        Desc.PixelFormat.BlueMask := FmtInfo.PixelFormat.BBitMask;
      end;
    end;

    // Header and main image are written to output
    Write(Handle, @Hdr, SizeOf(Hdr));
    Write(Handle, MainImage.Bits, MainImage.Size);

    // Write the rest of the images and convert them to
    // the same format as main image if necessary and ensure proper mipmap
    // simensions too.
    for I := MainIdx + 1 to MainIdx + ImageCount - 1 do
    begin
      // Get proper dimensions for this level
      ComputeSubDimensions(I, Desc.Width, Desc.Height, Desc.MipMaps, Desc.Depth,
        IsCubeMap, IsVolume, CurrentWidth, CurrentHeight);

      // Check if input image for this level has the right size and format
      NeedsResize := not ((Images[I].Width = CurrentWidth) and (Images[I].Height = CurrentHeight));
      NeedsConvert := not (Images[I].Format = Format);

      if NeedsResize or NeedsConvert then
      begin
        // Input image must be resized or converted to different format
        // to become valid mipmap level
        InitImage(ImageToSave);
        CloneImage(Images[I], ImageToSave);
        if NeedsConvert then
          ConvertImage(ImageToSave, Format);
        if NeedsResize then
          ResizeImage(ImageToSave, CurrentWidth, CurrentHeight, rfBilinear);
      end
      else
        // Input image can be used without any changes
        ImageToSave := Images[I];

      // Write level data and release temp image if necessary
      Write(Handle, ImageToSave.Bits, ImageToSave.Size);
      if Images[I].Bits <> ImageToSave.Bits then
        FreeImage(ImageToSave);
    end;

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(MainImage);
  end;
end;

procedure TDDSFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsIndexed or Info.IsSpecial then
    // convert indexed and unsupported special formatd to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else if Info.IsFloatingPoint then
  begin
    if Info.Format = ifA16R16G16B16F then
      // only swap channels here
      ConvFormat := ifA16B16G16R16F
    else
      // convert other floating point formats to A32B32G32R32F
      ConvFormat := ifA32B32G32R32F
  end
  else if Info.HasGrayChannel then
  begin
    if Info.HasAlphaChannel then
      // convert grayscale with alpha to A8Gray8
      ConvFormat := ifA8Gray8
    else if Info.BytesPerPixel = 1 then
      // convert 8bit grayscale to Gray8
      ConvFormat := ifGray8
    else
      // convert 16-64bit grayscales to Gray16
      ConvFormat := ifGray16;
  end
  else if Info.BytesPerPixel > 4 then
    ConvFormat := ifA16B16G16R16
  else if Info.HasAlphaChannel then
    // convert the other images with alpha channel to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else
    // convert the other formats to X8R8G8B8
    ConvFormat := ifX8R8G8B8;

  ConvertImage(Image, ConvFormat);
end;

function TDDSFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TDDSFileHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
    with GetIO do
    begin
      ReadCount := Read(Handle, @Hdr, SizeOf(Hdr));
      Seek(Handle, -ReadCount, smFromCurrent);
      Result := (Hdr.Magic = DDSMagic) and (ReadCount = SizeOf(Hdr)) and
        ((Hdr.Desc.Caps.Caps1 and DDSCAPS_TEXTURE) = DDSCAPS_TEXTURE);
    end;
end;

initialization
  RegisterImageFileFormat(TDDSFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.1 ----------------------------------------------------
    - Texture and D3D specific info stored in DDS is now available as metadata
      (loading).
    - Added support for loading DDS files with DX10 extension
      (http://msdn.microsoft.com/en-us/library/windows/desktop/bb943991(v=vs.85).aspx)
      and few compatibility fixes.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Added support for 3Dc ATI1/2 formats.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Saved DDS with mipmaps now correctly defineds COMPLEX flag.
    - Fixed loading of RGB DDS files that use pitch and have mipmaps -
      mipmaps were loaded wrongly.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Changed saving behaviour a bit: mipmaps are inlcuded automatically for
      2D textures if input image array has more than 1 image (no need to
      set SaveMipMapCount manually).
    - Mipmap levels are now saved with proper dimensions when saving DDS files.
    - Made some changes to not be so strict when loading DDS files.
      Many programs seem to save them in non-standard format
      (by MS DDS File Reference).
    - Added missing ifX8R8G8B8 to SupportedFormats, MakeCompatible failed
      when image was converted to this format (inside).
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Fixed bug that sometimes saved non-standard DDS files and another
      one that caused crash when these files were loaded.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added support for half-float image formats
    - change in LoadData to allow support for more images
      in one stream loading

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - fixed bug in TestFormat which does not recognize many DDS files
    - changed pitch/linearsize handling in DDS loading code to
      load DDS files produced by NVidia's Photoshop plugin
}

end.

