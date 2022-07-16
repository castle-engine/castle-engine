{
  Vampyre Imaging Library
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ This is import wrapper for Delphi.NET. You need VampyreImaging.dll
  located somewhere Windows can find it. You can use functions directly
  imported from DLL or much more dotNET-like Imaging class members.

  Note that this wrapper was not tested extensively so there may be various bugs.}
unit ImagingNET;

{$MINENUMSIZE 4}

interface

uses
  System.Runtime.InteropServices, System.Security, System.Text, SysUtils;

const
  ImagingVersionMajor = 0;
  ImagingVersionMinor = 24;
  ImagingVersionPatch = 0;

  ImagingJpegQuality           = 10;
  ImagingJpegProgressive       = 11;
  ImagingBitmapRLE             = 12;
  ImagingTargaRLE              = 13;
  ImagingDDSLoadedCubeMap      = 14;
  ImagingDDSLoadedVolume       = 15;
  ImagingDDSLoadedMipMapCount  = 16;
  ImagingDDSLoadedDepth        = 17;
  ImagingDDSSaveCubeMap        = 18;
  ImagingDDSSaveVolume         = 19;
  ImagingDDSSaveMipMapCount    = 20;
  ImagingDDSSaveDepth          = 21;
  ImagingPNGPreFilter          = 25;
  ImagingPNGCompressLevel      = 26;
  ImagingMNGLossyCompression   = 28;
  ImagingMNGLossyAlpha         = 29;
  ImagingMNGPreFilter          = 30;
  ImagingMNGCompressLevel      = 31;
  ImagingMNGQuality            = 32;
  ImagingMNGProgressive        = 33;
  ImagingJNGLossyAlpha         = 40;
  ImagingJNGAlphaPreFilter     = 41;
  ImagingJNGAlphaCompressLevel = 42;
  ImagingJNGQuality            = 43;
  ImagingJNGProgressive        = 44;
  ImagingPGMSaveBinary         = 50;
  ImagingPPMSaveBinary         = 51;

  ImagingJpeg2000Quality             = 55;
  ImagingJpeg2000CodeStreamOnly      = 56;
  ImagingJpeg2000LosslessCompression = 57;
  ImagingJpeg2000ScaleOutput         = 58;
  ImagingTiffCompression             = 65;

  ImagingColorReductionMask   = 128;
  ImagingLoadOverrideFormat   = 129;
  ImagingSaveOverrideFormat   = 130;
  ImagingMipMapFilter         = 131;

  InvalidOption               = -$7FFFFFFF;

  ChannelBlue  = 0;
  ChannelGreen = 1;
  ChannelRed   = 2;
  ChannelAlpha = 3;

type
  TImageFormat = (
    ifUnknown        = 0,
    ifDefault        = 1,
    // indexed formats using palette
    ifIndex8         = 10,
    // grayscale formats
    ifGray8          = 40,
    ifA8Gray8        = 41,
    ifGray16         = 42,
    ifGray32         = 43,
    ifGray64         = 44,
    ifA16Gray16      = 45,
    // ARGB formats
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
    // floating point formats
    ifR32F           = 170,
    ifA32R32G32B32F  = 171,
    ifA32B32G32R32F  = 172,
    ifR16F           = 173,
    ifA16R16G16B16F  = 174,
    ifA16B16G16R16F  = 175,
    // special formats
    ifDXT1           = 220,
    ifDXT3           = 221,
    ifDXT5           = 222,
    ifBTC            = 223);

  TColor32 = UInt32;
  TColor64 = UInt64;

  TPalette32 = IntPtr;
  TImageDataList = UInt32;

  [StructLayout(LayoutKind.Sequential)]
  TImageData = packed record
    Width: LongInt;
    Height: LongInt;
    Format: TImageFormat;
    Size: LongInt;
    Bits: IntPtr;
    Palette: TPalette32;
  end;

  [StructLayout(LayoutKind.Sequential)]
  TImageFormatInfo = packed record
    Format: TImageFormat;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 16)]
    Name: string;
    BytesPerPixel: Byte;
    PaletteEntries: LongInt;
    HasGrayChannel: Boolean;
    HasAlphaChannel: Boolean;
    IsFloatingPoint: Boolean;
    UsePixelFormat: Boolean;
    IsRBSwapped: Boolean;
    IsIndexed: Boolean;
    IsSpecial: Boolean;
    PixelFormat: IntPtr;
    GetPixelsSize: IntPtr;
    CheckDimensions: IntPtr;
    GetPixel32: IntPtr;      
    GetPixelFP: IntPtr;
    SetPixel32: IntPtr;
    SetPixelFP: IntPtr;
  end;

  TResizeFilter = (
    rfNearest  = 0,
    rfBilinear = 1,
    rfBicubic  = 2);

  TColor24Rec = record;
  TColor32Rec = record;
  TColor48Rec = record;
  TColor64Rec = record;
  TColorFPRec = record;

  TColor24Rec = packed record
  public
     B, G, R: Byte;
     function SetColor(Color: TColor32): TColor24Rec; overload;
     function SetColor(R, G, B: Byte): TColor24Rec; overload;
     function SetColor(ColorRec: TColor24Rec): TColor24Rec; overload;
     function SetColor(ColorRec: TColor32Rec): TColor24Rec; overload;
     function SetColor(ColorRec: TColor48Rec): TColor24Rec; overload;
     function SetColor(ColorRec: TColor64Rec): TColor24Rec; overload;
     function SetColor(ColorRec: TColorFPRec): TColor24Rec; overload;
     function GetColor: TColor32;
     class operator Equal(const Left, Right: TColor24Rec): Boolean;
     class operator NotEqual(const Left, Right: TColor24Rec): Boolean;
  end;

  TColor32Rec = packed record
  public
     B, G, R, A: Byte;
     function SetColor(Color: TColor32): TColor32Rec; overload;
     function SetColor(A, R, G, B: Byte): TColor32Rec; overload;
     function SetColor(ColorRec: TColor24Rec): TColor32Rec; overload;
     function SetColor(ColorRec: TColor32Rec): TColor32Rec; overload;
     function SetColor(ColorRec: TColor48Rec): TColor32Rec; overload;
     function SetColor(ColorRec: TColor64Rec): TColor32Rec; overload;
     function SetColor(ColorRec: TColorFPRec): TColor32Rec; overload;
     function GetColor: TColor32;
     class operator Equal(const Left, Right: TColor32Rec): Boolean;
     class operator NotEqual(const Left, Right: TColor32Rec): Boolean;
  end;

  TColor48Rec = packed record
  public
     B, G, R: Word;
     function SetColor(Color: TColor64): TColor48Rec; overload;
     function SetColor(R, G, B: Word): TColor48Rec; overload;
     function SetColor(ColorRec: TColor24Rec): TColor48Rec; overload;
     function SetColor(ColorRec: TColor32Rec): TColor48Rec; overload;
     function SetColor(ColorRec: TColor48Rec): TColor48Rec; overload;
     function SetColor(ColorRec: TColor64Rec): TColor48Rec; overload;
     function SetColor(ColorRec: TColorFPRec): TColor48Rec; overload;
     function GetColor: TColor64;
     class operator Equal(const Left, Right: TColor48Rec): Boolean;
     class operator NotEqual(const Left, Right: TColor48Rec): Boolean;
  end;

  TColor64Rec = packed record
  public
     B, G, R, A: Word;
     function SetColor(Color: TColor64): TColor64Rec; overload;
     function SetColor(A, R, G, B: Word): TColor64Rec; overload;
     function SetColor(ColorRec: TColor24Rec): TColor64Rec; overload;
     function SetColor(ColorRec: TColor32Rec): TColor64Rec; overload;
     function SetColor(ColorRec: TColor48Rec): TColor64Rec; overload;
     function SetColor(ColorRec: TColor64Rec): TColor64Rec; overload;
     function SetColor(ColorRec: TColorFPRec): TColor64Rec; overload;
     function GetColor: TColor64;
     class operator Equal(const Left, Right: TColor64Rec): Boolean;
     class operator NotEqual(const Left, Right: TColor64Rec): Boolean;
  end;

  TColorFPRec = packed record
  public
     B, G, R, A: Single;
     function SetColor(Color: TColor64): TColorFPRec; overload;
     function SetColor(A, R, G, B: Single): TColorFPRec; overload;
     function SetColor(ColorRec: TColor24Rec): TColorFPRec; overload;
     function SetColor(ColorRec: TColor32Rec): TColorFPRec; overload;
     function SetColor(ColorRec: TColor48Rec): TColorFPRec; overload;
     function SetColor(ColorRec: TColor64Rec): TColorFPRec; overload;
     function SetColor(ColorRec: TColorFPRec): TColorFPRec; overload;
     function GetColor: TColor64;
     class operator Equal(const Left, Right: TColorFPRec): Boolean;
     class operator NotEqual(const Left, Right: TColorFPRec): Boolean;
  end;

  TDynImageDataArray = array of TImageData;

const
  LibraryName = 'VampyreImaging.dll';

{ Low Level Imported Functions }

{ General Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImGetVersion(var Major, Minor, Patch: LongInt); external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImInitImage(var Image: TImageData); external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImNewImage(Width, Height: LongInt; Format: TImageFormat;
  var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImTestImage(const Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFreeImage(var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImDetermineFileFormat(const FileName: string; Ext: StringBuilder): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImDetermineMemoryFormat(Data: array of Byte; Size: LongInt; Ext: StringBuilder): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImIsFileFormatSupported(const FileName: string): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImEnumFileFormats(var Index: LongInt; Name, DefaultExt, Masks: StringBuilder;
  var CanSave, IsMultiImageFormat: Boolean): Boolean; external;

{ Image List Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImInitImageList(Size: LongInt; var ImageList: TImageDataList): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetImageListSize(ImageList: TImageDataList): LongInt; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetImageListElement(ImageList: TImageDataList; Index: LongInt;
  var OutImage: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSetImageListSize(ImageList: TImageDataList; NewSize: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSetImageListElement(ImageList: TImageDataList; Index: LongInt;
  var InImage: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImTestImagesInList(ImageList: TImageDataList): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFreeImageList(var ImageList: TImageDataList): Boolean; external;

{ Loading Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImLoadImageFromFile(const FileName: string; var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImLoadImageFromMemory(const Data: array of Byte; Size: LongInt;
  var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImLoadMultiImageFromFile(const FileName: string; var ImageList: TImageDataList): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImLoadMultiImageFromMemory(const Data: array of Byte; Size: LongInt;
  var ImageList: TImageDataList): Boolean; external;

{ Saving Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSaveImageToFile(const FileName: string; const Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSaveImageToMemory(const Ext: string; const Data: array of Byte; var Size: LongInt;
  const Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSaveMultiImageToFile(const FileName: string; ImageList: TImageDataList): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSaveMultiImageToMemory(const Ext: string; const Data: array of Byte; var Size: LongInt;
  ImageList: TImageDataList): Boolean; external;

{ Manipulation Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImCloneImage(const Image: TImageData; var Clone: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFlipImage(var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImMirrorImage(var Image: TImageData): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt; Filter: TResizeFilter): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TImageDataList): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImMapImageToPalette(var Image: TImageData; Pal: TPalette32;
  Entries: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSplitImage(var Image: TImageData; var Chunks: TImageDataList;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: IntPtr): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImMakePaletteForImages(var Images: TImageDataList; Pal: TPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImRotateImage(var Image: TImageData; Angle: Single): Boolean; external;

{ Drawing/Pixel functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImCopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFillRect(var Image: TImageData; X, Y, Width, Height: LongInt;
  Fill: IntPtr): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldPixel, NewPixel: IntPtr): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImStretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImGetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: IntPtr); external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImSetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: IntPtr); external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImSetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec); external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
procedure ImSetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); external;

{ Palette Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImNewPalette(Entries: LongInt; var Pal: TPalette32): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFreePalette(var Pal: TPalette32): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImCopyPalette(SrcPal, DstPal: TPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFindColor(Pal: TPalette32; Entries: LongInt; Color: TColor32): LongInt; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFillGrayscalePalette(Pal: TPalette32; Entries: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImFillCustomPalette(Pal: TPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSwapChannelsOfPalette(Pal: TPalette32; Entries, SrcChannel,
  DstChannel: LongInt): Boolean; external;

{ Options Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImSetOption(OptionId, Value: LongInt): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetOption(OptionId: LongInt): LongInt; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImPushOptions: Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImPopOptions: Boolean; external;

{ Image Format Functions }

[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetPixelBytes(Format: TImageFormat): LongInt; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean; external;
[SuppressUnmanagedCodeSecurity, DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
function ImGetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; external;

type
  { Record with information about one of imag file formats supported by Imaging.}
  TFileFormatInfo = record
    Name: string;
    DefaultExt: string;
    Masks: string;
    CanSave: Boolean;
    IsMultiImageFormat: Boolean;
  end;

  { Class which encapsulates all Imaging functions without Im prefix and
    with dotNET friendly parameter types. There are also some dotNET only
    members.}
  Imaging = class(TObject)
  public
    class var FileFormats: array of TFileFormatInfo;
    class procedure BuildFileFormatList;
    class function EnumFileFormats(var Index: LongInt; var Name, DefaultExt, Masks: string; var CanSave, IsMultiImageFormat: Boolean): Boolean; static;
    class function ListToArray(List: TImageDataList; var Arr: TDynImageDataArray): Boolean; static;
    class function ArrayToList(const Arr: TDynImageDataArray; var List: TImageDataList): Boolean; static;
  public
    { General Functions }
    class procedure GetVersion(var Major, Minor, Patch: LongInt); static;
    class procedure InitImage(var Image: TImageData); static;
    class function NewImage(Width, Height: LongInt; Format: TImageFormat; var Image: TImageData): Boolean; static;
    class function TestImage(const Image: TImageData): Boolean; static;
    class function FreeImage(var Image: TImageData): Boolean; static;
    class function FreeImagesInArray(var Images: TDynImageDataArray): Boolean; static;
    class function TestImagesInArray(const Images: TDynImageDataArray): Boolean; static;
    class function DetermineFileFormat(const FileName: string): string; static;
    class function DetermineMemoryFormat(const Data: array of Byte): string; static;
    class function IsFileFormatSupported(const FileName: string): Boolean; static;
    class function GetFileFormatCount: LongInt; static;
    class function GetFileFormatInfo(Index: LongInt): TFileFormatInfo; static;
    class function GetImageFileFormatsFilter(OpenFileFilter: Boolean): string; static;
    { Loading Functions }
    class function LoadImageFromFile(const FileName: string; var Image: TImageData): Boolean; static;
    class function LoadImageFromMemory(const Data: array of Byte; var Image: TImageData): Boolean; static;
    class function LoadMultiImageFromFile(const FileName: string; var Images: TDynImageDataArray): Boolean; static;
    class function LoadMultiImageFromMemory(const Data: array of Byte; var Images: TDynImageDataArray): Boolean; static;
    { Saving Functions }
    class function SaveImageToFile(const FileName: string; const Image: TImageData): Boolean; static;
    class function SaveImageToMemory(const Ext: string; Data: array of Byte; var Size: LongInt; const Image: TImageData): Boolean; static;
    class function SaveMultiImageToFile(const FileName: string; const Images: TDynImageDataArray): Boolean; static;
    class function SaveMultiImageToMemory(const Ext: string; Data: array of Byte; var Size: LongInt; const Images: TDynImageDataArray): Boolean; static;
    { Manipulation Functions }
    class function CloneImage(const Image: TImageData; var Clone: TImageData): Boolean; static;
    class function ConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean; static;
    class function FlipImage(var Image: TImageData): Boolean; static;
    class function MirrorImage(var Image: TImageData): Boolean; static;
    class function ResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt; Filter: TResizeFilter): Boolean; static;
    class function SwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean; static;
    class function ReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean; static;
    class function GenerateMipMaps(const Image: TImageData; Levels: LongInt; var MipMaps: TDynImageDataArray): Boolean; static;
    class function MapImageToPalette(var Image: TImageData; Pal: TPalette32; Entries: LongInt): Boolean; static;
    class function SplitImage(var Image: TImageData; var Chunks: TDynImageDataArray; ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt; PreserveSize: Boolean; Fill: TObject): Boolean; static;
    class function MakePaletteForImages(var Images: TDynImageDataArray; Pal: TPalette32; MaxColors: LongInt; ConvertImages: Boolean): Boolean; static;
    class function RotateImage(var Image: TImageData; Angle: Single): Boolean; static;
    { Drawing/Pixel functions }
    class function CopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt; var DstImage: TImageData; DstX, DstY: LongInt): Boolean; static;
    class function FillRect(var Image: TImageData; X, Y, Width, Height: LongInt; Fill: TObject): Boolean; static;
    class function ReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt; OldPixel, NewPixel: TObject): Boolean; static;
    class function StretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth, SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth, DstHeight: LongInt; Filter: TResizeFilter): Boolean; static;
    class procedure GetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: TObject); static;
    class procedure SetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: TObject); static;
    class function GetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec; static;
    class procedure SetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec); static;
    class function GetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec; static;
    class procedure SetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); static;
    { Palette Functions }
    class function NewPalette(Entries: LongInt; var Pal: TPalette32): Boolean; static;
    class function FreePalette(var Pal: TPalette32): Boolean; static;
    class function CopyPalette(SrcPal, DstPal: TPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean; static;
    class function FindColor(Pal: TPalette32; Entries: LongInt; Color: TColor32): LongInt; static;
    class function FillGrayscalePalette(Pal: TPalette32; Entries: LongInt): Boolean; static;
    class function FillCustomPalette(Pal: TPalette32; Entries: LongInt; RBits, GBits, BBits: Byte; Alpha: Byte): Boolean; static;
    class function SwapChannelsOfPalette(Pal: TPalette32; Entries, SrcChannel, DstChannel: LongInt): Boolean; static;
    {  dotNET only}
    class function GetPaletteColor(Pal: TPalette32; Index: LongInt): TColor32; static;
    class procedure SetPaletteColor(Pal: TPalette32; Index: LongInt; Color: TColor32); static;
    { Options Functions }
    class function SetOption(OptionId, Value: LongInt): Boolean; static;
    class function GetOption(OptionId: LongInt): LongInt; static;
    class function PushOptions: Boolean; static;
    class function PopOptions: Boolean; static;
    { Image Format Functions }
    class function GetPixelBytes(Format: TImageFormat): LongInt; static;
    class function GetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean; static;
    class function GetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; static;
  end;

implementation

{ Imaging private methods }

class function Imaging.ListToArray(List: TImageDataList; var Arr: TDynImageDataArray): Boolean;
var
  Img: TImageData;
  I, Size: LongInt;
begin
  Result := True;
  Size := ImGetImageListSize(List);
  FreeImagesInArray(Arr);
  SetLength(Arr, Size);
  for I := 0 to Size - 1 do
  begin
    Result := Result and ImGetImageListElement(List, I, Img);
    Arr[I] := Img;
    if not Result then Break;
  end;
end;

class function Imaging.ArrayToList(const Arr: TDynImageDataArray; var List: TImageDataList): Boolean;
var
  Img: TImageData;
  I, Size: LongInt;
begin
  Size := Length(Arr);
  Result := ImInitImageList(Size, List);
  for I := 0 to Size - 1 do
  begin
    Img := Arr[I];
    Result := Result and ImSetImageListElement(List, I, Img);
    if not Result then Break;
  end;
end;

{ Imaging public methods }

class procedure Imaging.GetVersion(var Major, Minor, Patch: LongInt);
begin
  ImGetVersion(Major, Minor, Patch);
end;

class procedure Imaging.InitImage(var Image: TImageData);
begin
  ImInitImage(Image);
end;

class function Imaging.IsFileFormatSupported(const FileName: string): Boolean;
begin
  Result := IsFileFormatSupported(FileName);
end;

class function Imaging.NewImage(Width, Height: LongInt; Format: TImageFormat; var Image: TImageData): Boolean;
begin
  Result := ImNewImage(Width, Height, Format, Image);
end;

class function Imaging.TestImage(const Image: TImageData): Boolean;
begin
  Result := ImTestImage(Image);
end;

class function Imaging.FreeImage(var Image: TImageData): Boolean;
begin
  Result := ImFreeImage(Image);
end;

class function Imaging.FreeImagesInArray(var Images: TDynImageDataArray): Boolean;
var
  I: LongInt;
begin
  Result := True;
  for I := 0 to Length(Images) - 1 do
    Result := Result and ImFreeImage(Images[I]);
end;

class function Imaging.TestImagesInArray(const Images: TDynImageDataArray): Boolean;
var
  I: LongInt;
begin
  Result := True;
  for I := 0 to Length(Images) - 1 do
  begin
    Result := Result and ImTestImage(Images[I]);
    if not Result then Break;
  end;
end;

const
  ExtLen = 16;

class function Imaging.DetermineFileFormat(const FileName: string): string;
var
  Builder: StringBuilder;
begin
  Builder := StringBuilder.Create(ExtLen);
  if ImDetermineFileFormat(FileName, Builder) then
    Result := Builder.ToString
  else
    Result := '';
end;

class function Imaging.DetermineMemoryFormat(const Data: array of Byte): string;
var
  Builder: StringBuilder;
begin
  Builder := StringBuilder.Create(ExtLen);
  if ImDetermineMemoryFormat(Data, Length(Data), Builder) then
    Result := Builder.ToString
  else
    Result := '';
end;

class function Imaging.EnumFileFormats(var Index: Integer; var Name, DefaultExt,
  Masks: string; var CanSave, IsMultiImageFormat: Boolean): Boolean;
var
  AName, AExt, AMasks: StringBuilder;
begin
  AName := StringBuilder.Create(128);
  AExt := StringBuilder.Create(ExtLen);
  AMasks := StringBuilder.Create(256);

  Result := ImEnumFileFormats(Index, AName, AExt, AMasks, CanSave, IsMultiImageFormat);
  // TODO: Result always is True, even if DLL function explicitly returns False. WTF?
  // So this check is added to ensure enumerating will end some time.
  Result := Result and (AName.Length > 0);

  Name := AName.ToString;
  DefaultExt := AExt.ToString;
  Masks := AMasks.ToString;
end;

class function Imaging.LoadImageFromFile(const FileName: string; var Image: TImageData): Boolean;
begin
  Result := ImLoadImageFromFile(FileName, Image);
end;

class function Imaging.LoadImageFromMemory(const Data: array of Byte; var Image: TImageData): Boolean;
begin
  Result := ImLoadImageFromMemory(Data, Length(Data), Image);
end;

class function Imaging.LoadMultiImageFromFile(const FileName: string; var Images: TDynImageDataArray): Boolean;
var
  List: TImageDataList;
begin
  Result := ImLoadMultiImageFromFile(FileName, List);
  if Result then
  begin
    FreeImagesInArray(Images);
    Result := ListToArray(List, Images);
    ImFreeImageList(List);
  end;
end;

class function Imaging.LoadMultiImageFromMemory(const Data: array of Byte; var Images: TDynImageDataArray): Boolean;
var
  List: TImageDataList;
begin
  Result := ImLoadMultiImageFromMemory(Data, Length(Data), List);
  if Result then
  begin
    FreeImagesInArray(Images);
    Result := ListToArray(List, Images);
    ImFreeImageList(List);
  end;
end;

class function Imaging.SaveImageToFile(const FileName: string; const Image: TImageData): Boolean;
begin
  Result := ImSaveImageToFile(FileName, Image);
end;

class function Imaging.SaveImageToMemory(const Ext: string; Data: array of Byte; var Size: LongInt; const Image: TImageData): Boolean;
begin
  Size := Length(Data);
  Result := ImSaveImageToMemory(Ext, Data, Size, Image);
end;

class function Imaging.SaveMultiImageToFile(const FileName: string; const Images: TDynImageDataArray): Boolean;
var
  List: TImageDataList;
begin
  Result := ArrayToList(Images, List);
  if Result then
  begin
    Result := ImSaveMultiImageToFile(FileName, List);
    ImFreeImageList(List);
  end;
end;

class function Imaging.SaveMultiImageToMemory(const Ext: string; Data: array of Byte; var Size: LongInt; const Images: TDynImageDataArray): Boolean;
var
  List: TImageDataList;
begin
  Result := ArrayToList(Images, List);
  if Result then
  begin
    Size := Length(Data);
    Result := ImSaveMultiImageToMemory(Ext, Data, Size, List);
    ImFreeImageList(List);
  end;
end;

class procedure Imaging.BuildFileFormatList;
var
  I: LongInt;
begin
  I := 0;
  SetLength(FileFormats, 1);
  while Imaging.EnumFileFormats(I, FileFormats[I].Name, FileFormats[I].DefaultExt,
    FileFormats[I].Masks, FileFormats[I].CanSave, FileFormats[I].IsMultiImageFormat) do
  begin
    SetLength(FileFormats, I + 1);
  end;
  SetLength(FileFormats, I);
end;

class function Imaging.CloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
begin
  Result := ImCloneImage(Image, Clone);
end;

class function Imaging.ConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean;
begin
  Result := ImConvertImage(Image, DestFormat);
end;

class function Imaging.FlipImage(var Image: TImageData): Boolean;
begin
  Result := ImFlipImage(Image);
end;

class function Imaging.MirrorImage(var Image: TImageData): Boolean;
begin
  Result := ImMirrorImage(Image);
end;

class function Imaging.ResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt; Filter: TResizeFilter): Boolean;
begin
  Result := ImResizeImage(Image, NewWidth, NewHeight, Filter);
end;

class function Imaging.SwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean;
begin
  Result := ImSwapChannels(Image, SrcChannel, DstChannel);
end;

class function Imaging.ReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean;
begin
  Result := ImReduceColors(Image, MaxColors);
end;

class function Imaging.GenerateMipMaps(const Image: TImageData; Levels: LongInt; var MipMaps: TDynImageDataArray): Boolean;
var
  List: TImageDataList;
begin
  Result := ImGenerateMipMaps(Image, Levels, List);
  if Result then
  begin
    FreeImagesInArray(MipMaps);
    Result := ListToArray(List, MipMaps);
    ImFreeImageList(List);
  end;
end;

class function Imaging.MapImageToPalette(var Image: TImageData; Pal: TPalette32; Entries: LongInt): Boolean;
begin
  Result := ImMapImageToPalette(Image, Pal, Entries);
end;

class function Imaging.SplitImage(var Image: TImageData; var Chunks: TDynImageDataArray; ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt; PreserveSize: Boolean; Fill: TObject): Boolean;
var
  Ptr: IntPtr;
  List: TImageDataList;
begin
  Ptr := Marshal.AllocHGlobal(Marshal.SizeOf(Fill));
  Marshal.StructureToPtr(Fill, Ptr, False);
  Result := ImSplitImage(Image, List, ChunkWidth, ChunkHeight, XChunks, YChunks,
    PreserveSize, Ptr);
  if Result then
  begin
    FreeImagesInArray(Chunks);
    Result := ListToArray(List, Chunks);
    ImFreeImageList(List);
  end;
  Marshal.FreeHGlobal(Ptr);
end;

class function Imaging.MakePaletteForImages(var Images: TDynImageDataArray; Pal: TPalette32; MaxColors: LongInt; ConvertImages: Boolean): Boolean;
var
  List: TImageDataList;
begin
  Result := ArrayToList(Images, List);
  if Result then
  begin
    Result := ImMakePaletteForImages(List, Pal, MaxColors, ConvertImages);
    Result := Result and ListToArray(List, Images);
    ImFreeImageList(List);
  end;
end;

class function Imaging.RotateImage(var Image: TImageData; Angle: Single): Boolean;
begin
  Result := ImRotateImage(Image, Angle);
end;

class function Imaging.CopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt; var DstImage: TImageData; DstX, DstY: LongInt): Boolean;
begin
  Result := ImCopyRect(SrcImage, SrcX, SrcY, Width, Height, DstImage, DstX, DstY);
end;

class function Imaging.FillRect(var Image: TImageData; X, Y, Width, Height: LongInt; Fill: TObject): Boolean;
var
  Ptr: IntPtr;
begin
  Ptr := Marshal.AllocHGlobal(Marshal.SizeOf(Fill));
  Marshal.StructureToPtr(Fill, Ptr, False);
  Result := ImFillRect(Image, X, Y, Width, Height, Ptr);
  Marshal.FreeHGlobal(Ptr);
end;

class function Imaging.ReplaceColor(var Image: TImageData; X, Y, Width,
  Height: LongInt; OldPixel, NewPixel: TObject): Boolean;
var
  OldPtr, NewPtr: IntPtr;
begin
  OldPtr := Marshal.AllocHGlobal(Marshal.SizeOf(OldPixel));
  Marshal.StructureToPtr(OldPixel, OldPtr, False);
  NewPtr := Marshal.AllocHGlobal(Marshal.SizeOf(NewPixel));
  Marshal.StructureToPtr(NewPixel, NewPtr, False);
  Result := ReplaceColor(Image, X, Y, Width, Height, OldPtr, NewPtr);
  Marshal.FreeHGlobal(OldPtr);
  Marshal.FreeHGlobal(NewPtr);
end;

class function Imaging.StretchRect(const SrcImage: TImageData; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: Integer; Filter: TResizeFilter): Boolean;
begin
  Result := ImStretchRect(SrcImage, SrcX, SrcY, SrcWidth, SrcHeight,
    DstImage, DstX, DstY, DstWidth, DstHeight, Filter);
end;

class procedure Imaging.GetPixelDirect(const Image: TImageData; X, Y: Integer;
  Pixel: TObject);
var
  Ptr: IntPtr;
begin
  Ptr := Marshal.AllocHGlobal(Marshal.SizeOf(Pixel));
  ImGetPixelDirect(Image, X, Y, Ptr);
  Marshal.PtrToStructure(Ptr, Pixel);
  Marshal.FreeHGlobal(Ptr);
end;

class function Imaging.GetPixel32(const Image: TImageData; X,
  Y: Integer): TColor32Rec;
begin
  Result := ImGetPixel32(Image, X, Y);
end;

class procedure Imaging.SetPixel32(const Image: TImageData; X, Y: Integer;
  const Color: TColor32Rec);
begin
  ImSetPixel32(Image, X, Y, Color);
end;

class function Imaging.GetPixelFP(const Image: TImageData; X,
  Y: Integer): TColorFPRec;
begin
  Result := ImGetPixelFP(Image, X, Y);
end;

class procedure Imaging.SetPixelDirect(const Image: TImageData; X, Y: Integer;
  Pixel: TObject);
var
  Ptr: IntPtr;
begin
  Ptr := Marshal.AllocHGlobal(Marshal.SizeOf(Pixel));
  Marshal.StructureToPtr(Pixel, Ptr, False);
  ImSetPixelDirect(Image, X, Y, Ptr);
  Marshal.FreeHGlobal(Ptr);
end;

class procedure Imaging.SetPixelFP(const Image: TImageData; X, Y: Integer;
  const Color: TColorFPRec);
begin
  ImSetPixelFP(Image, X, Y, Color);
end;

class function Imaging.NewPalette(Entries: LongInt; var Pal: TPalette32): Boolean;
begin
  Result := ImNewPalette(Entries, Pal);
end;

class function Imaging.FreePalette(var Pal: TPalette32): Boolean;
begin
  Result := ImFreePalette(Pal);
end;

class function Imaging.CopyPalette(SrcPal, DstPal: TPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean;
begin
  Result := ImCopyPalette(SrcPal, DstPal, SrcIdx, DstIdx, Count);
end;

class function Imaging.FindColor(Pal: TPalette32; Entries: LongInt; Color: TColor32): LongInt;
begin
  Result := ImFindColor(Pal, Entries, Color);
end;

class function Imaging.FillGrayscalePalette(Pal: TPalette32; Entries: LongInt): Boolean;
begin
  Result := ImFillGrayscalePalette(Pal, Entries);
end;

class function Imaging.FillCustomPalette(Pal: TPalette32; Entries: LongInt; RBits, GBits, BBits: Byte; Alpha: Byte): Boolean;
begin
  Result := ImFillCustomPalette(Pal, Entries, RBits, GBits, BBits, Alpha);
end;

class function Imaging.SwapChannelsOfPalette(Pal: TPalette32; Entries, SrcChannel, DstChannel: LongInt): Boolean;
begin
  Result := ImSwapChannelsOfPalette(Pal, Entries, SrcChannel, DstChannel);
end;

class function Imaging.GetPaletteColor(Pal: TPalette32; Index: LongInt): TColor32;
begin
  Result := Marshal.ReadInt32(Pal, Index * SizeOf(TColor32));
end;

class procedure Imaging.SetPaletteColor(Pal: TPalette32; Index: LongInt; Color: TColor32);
begin
  Marshal.WriteInt32(Pal, Index * SizeOf(TColor32), Color);
end;

class function Imaging.SetOption(OptionId, Value: LongInt): Boolean;
begin
  Result := ImSetOption(OptionId, Value);
end;

class function Imaging.GetOption(OptionId: LongInt): LongInt;
begin
  Result := ImGetOption(OptionId);
end;

class function Imaging.PushOptions: Boolean;
begin
  Result := ImPushOptions;
end;

class function Imaging.PopOptions: Boolean;
begin
  Result := ImPopOptions;
end;

class function Imaging.GetPixelBytes(Format: TImageFormat): LongInt;
begin
  Result := ImGetPixelBytes(Format);
end;

class function Imaging.GetFileFormatCount: LongInt;
begin
  Result := Length(FileFormats);
end;

class function Imaging.GetFileFormatInfo(Index: LongInt): TFileFormatInfo;
begin
  if (Index >= Low(FileFormats)) and (Index <= High(FileFormats)) then
    Result := FileFormats[Index];
end;

class function Imaging.GetImageFileFormatsFilter(
  OpenFileFilter: Boolean): string;
const
  SAllFilter = 'All Images';
var
  I, Count: LongInt;
  Descriptions: string;
  Filters, CurFilter: string;
begin
  Descriptions := '';
  Filters := '';
  Count := 0;
  for I := 0 to Length(FileFormats) - 1 do
  begin
    if not OpenFileFilter and not FileFormats[I].CanSave then
      Continue;
    CurFilter := FileFormats[I].Masks;
    FmtStr(Descriptions, '%s%s (%s)|%2:s', [Descriptions, FileFormats[I].Name, CurFilter]);
    FmtStr(Filters, '%s;%s', [Filters, CurFilter]);
    if I < Length(FileFormats) - 1 then
        Descriptions := Descriptions + '|';
    Inc(Count);
  end;

  if (Count > 1) and OpenFileFilter then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [SAllFilter, Filters, Descriptions]);
  Result := Descriptions;
end;

class function Imaging.GetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean;
begin
  Result := ImGetImageFormatInfo(Format, Info);
end;

class function Imaging.GetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  Result := ImGetPixelsSize(Format, Width, Height);
end;

{ Color Records implementations }

{ TColor24Rec }

function TColor24Rec.SetColor(Color: TColor32): TColor24Rec;
begin
  R := (Color shr 16) and $FF;
  G := (Color shr 8) and $FF;
  B := Color and $FF;
  Result := Self;
end;

function TColor24Rec.GetColor: TColor32;
begin
  Result := ($FF shl 24) or (R shl 16) or (G shl 8) or B;
end;

function TColor24Rec.SetColor(R, G, B: Byte): TColor24Rec;
begin
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Result := Self;
end;

function TColor24Rec.SetColor(ColorRec: TColorFPRec): TColor24Rec;
begin
  Self.R := Math.Max(0, Math.Min(255, Trunc(ColorRec.R * 255)));
  Self.G := Math.Max(0, Math.Min(255, Trunc(ColorRec.G * 255)));
  Self.B := Math.Max(0, Math.Min(255, Trunc(ColorRec.B * 255)));
  Result := Self;
end;

function TColor24Rec.SetColor(ColorRec: TColor64Rec): TColor24Rec;
begin
  Self.R := ColorRec.R shr 8;
  Self.G := ColorRec.G shr 8;
  Self.B := ColorRec.B shr 8;
  Result := Self;
end;

function TColor24Rec.SetColor(ColorRec: TColor48Rec): TColor24Rec;
begin
  Self.R := ColorRec.R shr 8;
  Self.G := ColorRec.G shr 8;
  Self.B := ColorRec.B shr 8;
  Result := Self;
end;

function TColor24Rec.SetColor(ColorRec: TColor32Rec): TColor24Rec;
begin
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor24Rec.SetColor(ColorRec: TColor24Rec): TColor24Rec;
begin
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

class operator TColor24Rec.Equal(const Left, Right: TColor24Rec): Boolean;
begin
  Result := (Left.R = Right.R) and (Left.G = Right.G) and (Left.B = Right.B);
end;

class operator TColor24Rec.NotEqual(const Left, Right: TColor24Rec): Boolean;
begin
  Result := not TColor24Rec.Equals(Left, Right);
end;

{ TColor32Rec }

function TColor32Rec.SetColor(Color: TColor32): TColor32Rec;
begin
  A := Color shr 24;
  R := (Color shr 16) and $FF;
  G := (Color shr 8) and $FF;
  B := Color and $FF;
  Result := Self;
end;

function TColor32Rec.GetColor: TColor32;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

function TColor32Rec.SetColor(A, R, G, B: Byte): TColor32Rec;
begin
  Self.A := A;
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Result := Self;
end;

function TColor32Rec.SetColor(ColorRec: TColorFPRec): TColor32Rec;
begin
  Self.A := Math.Max(0, Math.Min($FF, Trunc(ColorRec.A * $FF)));
  Self.R := Math.Max(0, Math.Min($FF, Trunc(ColorRec.R * $FF)));
  Self.G := Math.Max(0, Math.Min($FF, Trunc(ColorRec.G * $FF)));
  Self.B := Math.Max(0, Math.Min($FF, Trunc(ColorRec.B * $FF)));
  Result := Self;
end;

function TColor32Rec.SetColor(ColorRec: TColor64Rec): TColor32Rec;
begin
  Self.A := ColorRec.A shr 8;
  Self.R := ColorRec.R shr 8;
  Self.G := ColorRec.G shr 8;
  Self.B := ColorRec.B shr 8;
  Result := Self;
end;

function TColor32Rec.SetColor(ColorRec: TColor48Rec): TColor32Rec;
begin
  Self.A := $FF;
  Self.R := ColorRec.R shr 8;
  Self.G := ColorRec.G shr 8;
  Self.B := ColorRec.B shr 8;
  Result := Self;
end;

function TColor32Rec.SetColor(ColorRec: TColor32Rec): TColor32Rec;
begin
  Self.A := ColorRec.A;
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor32Rec.SetColor(ColorRec: TColor24Rec): TColor32Rec;
begin
  Self.A := $FF;
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

class operator TColor32Rec.Equal(const Left, Right: TColor32Rec): Boolean;
begin
  Result := (Left.A = Right.A) and (Left.R = Right.R) and (Left.G = Right.G) and
    (Left.B = Right.B);
end;

class operator TColor32Rec.NotEqual(const Left, Right: TColor32Rec): Boolean;
begin
  Result := not TColor32Rec.Equals(Left, Right);
end;

{ TColor48Rec }

function TColor48Rec.SetColor(Color: TColor64): TColor48Rec;
begin
  R := (Color shr 32) and $FFFF;
  G := (Color shr 16) and $FFFF;
  B := Color and $FFFF;
  Result := Self;
end;

function TColor48Rec.GetColor: TColor64;
begin
  Result := (UInt64($FFFF) shl 48) or (UInt64(R) shl 32) or (G shl 16) or B;
end;

function TColor48Rec.SetColor(R, G, B: Word): TColor48Rec;
begin
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Result := Self;
end;

function TColor48Rec.SetColor(ColorRec: TColorFPRec): TColor48Rec;
begin
  Self.R := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.R * $FFFF)));
  Self.G := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.G * $FFFF)));
  Self.B := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.B * $FFFF)));
  Result := Self;
end;

function TColor48Rec.SetColor(ColorRec: TColor64Rec): TColor48Rec;
begin
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor48Rec.SetColor(ColorRec: TColor48Rec): TColor48Rec;
begin
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor48Rec.SetColor(ColorRec: TColor32Rec): TColor48Rec;
begin
  Self.R := ColorRec.R shl 8;
  Self.G := ColorRec.G shl 8;
  Self.B := ColorRec.B shl 8;
  Result := Self;
end;

function TColor48Rec.SetColor(ColorRec: TColor24Rec): TColor48Rec;
begin
  Self.R := ColorRec.R shl 8;
  Self.G := ColorRec.G shl 8;
  Self.B := ColorRec.B shl 8;
  Result := Self;
end;

class operator TColor48Rec.Equal(const Left, Right: TColor48Rec): Boolean;
begin
  Result := (Left.R = Right.R) and (Left.G = Right.G) and (Left.B = Right.B);
end;

class operator TColor48Rec.NotEqual(const Left, Right: TColor48Rec): Boolean;
begin
  Result := not TColor48Rec.Equals(Left, Right);
end;

{ TColor64Rec }

function TColor64Rec.SetColor(Color: TColor64): TColor64Rec;
begin
  A := Color shr 48;
  R := (Color shr 32) and $FFFF;
  G := (Color shr 16) and $FFFF;
  B := Color and $FFFF;
  Result := Self;
end;

function TColor64Rec.GetColor: TColor64;
begin
  Result := (UInt64(A) shl 48) or (UInt64(R) shl 32) or (G shl 16) or B;
end;

function TColor64Rec.SetColor(A, R, G, B: Word): TColor64Rec;
begin
  Self.A := A;
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Result := Self;
end;

function TColor64Rec.SetColor(ColorRec: TColorFPRec): TColor64Rec;
begin
  Self.A := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.A * $FFFF)));
  Self.R := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.R * $FFFF)));
  Self.G := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.G * $FFFF)));
  Self.B := Math.Max(0, Math.Min($FFFF, Trunc(ColorRec.B * $FFFF)));
  Result := Self;
end;

function TColor64Rec.SetColor(ColorRec: TColor64Rec): TColor64Rec;
begin
  Self.A := ColorRec.A;
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor64Rec.SetColor(ColorRec: TColor48Rec): TColor64Rec;
begin
  Self.A := $FFFF;
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColor64Rec.SetColor(ColorRec: TColor32Rec): TColor64Rec;
begin
  Self.A := ColorRec.A shl 8;
  Self.R := ColorRec.R shl 8;
  Self.G := ColorRec.G shl 8;
  Self.B := ColorRec.B shl 8;
  Result := Self;
end;

function TColor64Rec.SetColor(ColorRec: TColor24Rec): TColor64Rec;
begin
  Self.A := $FFFF;
  Self.R := ColorRec.R shl 8;
  Self.G := ColorRec.G shl 8;
  Self.B := ColorRec.B shl 8;
  Result := Self;
end;

class operator TColor64Rec.Equal(const Left, Right: TColor64Rec): Boolean;
begin
  Result := (Left.A = Right.A) and (Left.R = Right.R) and (Left.G = Right.G) and
    (Left.B = Right.B);
end;

class operator TColor64Rec.NotEqual(const Left, Right: TColor64Rec): Boolean;
begin
  Result := not TColor64Rec.Equals(Left, Right);
end;

{ TColorFRec }

function TColorFPRec.SetColor(Color: TColor64): TColorFPRec;
var
  C64: TColor64Rec;
begin
  C64.SetColor(Color);
  Self.SetColor(C64);
  Result := Self;
end;

function TColorFPRec.GetColor: TColor64;
var
  C64: TColor64Rec;
begin
  C64.SetColor(Self);
  Result := C64.GetColor;
end;

function TColorFPRec.SetColor(A, R, G, B: Single): TColorFPRec;
begin
  Self.A := A;
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Result := Self;
end;

function TColorFPRec.SetColor(ColorRec: TColorFPRec): TColorFPRec;
begin
  Self.A := ColorRec.A;
  Self.R := ColorRec.R;
  Self.G := ColorRec.G;
  Self.B := ColorRec.B;
  Result := Self;
end;

function TColorFPRec.SetColor(ColorRec: TColor64Rec): TColorFPRec;
begin
  Self.A := ColorRec.A / $FFFF;
  Self.R := ColorRec.R / $FFFF;
  Self.G := ColorRec.G / $FFFF;
  Self.B := ColorRec.B / $FFFF;
  Result := Self;
end;

function TColorFPRec.SetColor(ColorRec: TColor48Rec): TColorFPRec;
begin
  Self.A := 1.0;
  Self.R := ColorRec.R / $FFFF;
  Self.G := ColorRec.G / $FFFF;
  Self.B := ColorRec.B / $FFFF;
  Result := Self;
end;

function TColorFPRec.SetColor(ColorRec: TColor32Rec): TColorFPRec;
begin
  Self.A := ColorRec.A / $FF;
  Self.R := ColorRec.R / $FF;
  Self.G := ColorRec.G / $FF;
  Self.B := ColorRec.B / $FF;
  Result := Self;
end;

function TColorFPRec.SetColor(ColorRec: TColor24Rec): TColorFPRec;
begin
  Self.A := 1.0;
  Self.R := ColorRec.R / $FF;
  Self.G := ColorRec.G / $FF;
  Self.B := ColorRec.B / $FF;
  Result := Self;
end;

class operator TColorFPRec.Equal(const Left, Right: TColorFPRec): Boolean;
begin
  Result := (Left.A = Right.A) and (Left.R = Right.R) and (Left.G = Right.G) and
    (Left.B = Right.B);
end;

class operator TColorFPRec.NotEqual(const Left, Right: TColorFPRec): Boolean;
begin
  Result := not TColorFPRec.Equals(Left, Right);
end;

initialization
  Imaging.BuildFileFormatList;

{
  Changes/Bug Fixes:

  -- TODOS ----------------------------------------------------
    - add typecast operators to color records rather than SetColor methods 
    - add create System.Drawing.Bitmap from TImageData function

  -- 0.23 -----------------------------------------------------
    - Updated to DLL new version.

  -- 0.21 -----------------------------------------------------
    - Changed out PChar parameter types of imported functions to StringBuilders
      for easier conversions to System.String in Imaging class methods.
    - Added GetImageFileFormatFilter method to Imaging class.
    - Updated to DLL new version, some changes in Imaging class methods
      that return strings.

  -- 0.19 -----------------------------------------------------
    - updated to DLL new version

  -- 0.17 -----------------------------------------------------
    - added new low level functions and their equivalents in Imaging class,
      some old function headers updated

  -- 0.15 -----------------------------------------------------
    - changed headers of some functions because of changes in DLL
      mainly related to TImageDataList parameters. Few other
      "update" changes.

  -- 0.13 -----------------------------------------------------
    - Imaging class added
    - various color records implemented
    - several .NET only functions added
    - unit created
}

end.
