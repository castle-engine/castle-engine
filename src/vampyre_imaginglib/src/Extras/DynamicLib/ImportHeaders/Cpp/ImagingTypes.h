/*
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
*/

#ifndef IMAGINGTYPES_H
#define IMAGINGTYPES_H

#ifdef __cplusplus
namespace Imaging
{
  extern "C"
  {
#endif

#pragma pack(push, 1)

#if defined(__WIN32__) || defined(_WIN32)
  #define ImagingAPI __cdecl
#elif defined (linux) || defined (__linux__)
  #define ImagingAPI 
#endif

#define ImagingMajor 0
#define ImagingMinor 26
#define ImagingPatch 4

#define ImagingJpegQuality           10
#define ImagingJpegProgressive       11
#define ImagingBitmapRLE             12
#define ImagingTargaRLE              13
#define ImagingDDSLoadedCubeMap      14
#define ImagingDDSLoadedVolume       15
#define ImagingDDSLoadedMipMapCount  16
#define ImagingDDSLoadedDepth        17
#define ImagingDDSSaveCubeMap        18
#define ImagingDDSSaveVolume         19
#define ImagingDDSSaveMipMapCount    20
#define ImagingDDSSaveDepth          21
#define ImagingPNGPreFilter          25
#define ImagingPNGCompressLevel      26
#define ImagingMNGLossyCompression   28
#define ImagingMNGLossyAlpha         29
#define ImagingMNGPreFilter          30
#define ImagingMNGCompressLevel      31
#define ImagingMNGQuality            32
#define ImagingMNGProgressive        33
#define ImagingJNGLossyAlpha         40
#define ImagingJNGAlphaPreFilter     41
#define ImagingJNGAlphaCompressLevel 42
#define ImagingJNGQuality            43
#define ImagingJNGProgressive        44

#define ImagingJpeg2000Quality             55;
#define ImagingJpeg2000CodeStreamOnly      56;
#define ImagingJpeg2000LosslessCompression 57;
#define ImagingTiffCompression             65;

#define ImagingColorReductionMask    128
#define ImagingLoadOverrideFormat    129
#define ImagingSaveOverrideFormat    130
#define ImagingMipMapFilter          131

#define InvalidOption        -0x7FFFFFFF

#define ChannelBlue   0
#define ChannelGreen  1
#define ChannelRed    2
#define ChannelAlpha  3

#define False 0
#define True  1

typedef unsigned char Byte;
typedef Byte Boolean;
typedef unsigned short Word;
typedef unsigned long LongWord;
typedef void * Pointer;

typedef enum TImageFormat
{
  ifUnknown        = 0,
  ifDefault        = 1,
  /* indexed formats */
  ifIndex8         = 10,
  /* grayscale formats */
  ifGray8          = 40,
  ifA8Gray8        = 41,
  ifGray16         = 42,
  ifGray32         = 43,
  ifGray64         = 44,
  ifA16Gray16      = 45,
  /* ARGB formats */
  ifR1G1B1         = 80,
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
  /* floating-point formats */
  ifR32F           = 170,
  ifA32R32G32B32F  = 171,
  ifA32B32G32R32F  = 172,
  ifR16F           = 173,
  ifA16R16G16B16F  = 174,
  ifA16B16G16R16F  = 175,
  /* special formats */
  ifDXT1           = 220,
  ifDXT3           = 221,
  ifDXT5           = 222,
  ifBTC            = 223,
  /* dummy */
  ifForce32        = 0x7FFFFFFF
} TImageFormat;

typedef unsigned long TColor32;
typedef struct TColor64
{
  TColor32 Low;
  TColor32 High;
} TColor64;

typedef struct TColor24Rec
{
  union
  {
    struct
    {
      Byte B;
      Byte G;
      Byte R;
    };
    Byte Channels[3];
  };
} TColor24Rec;

typedef struct TColor32Rec
{
  union
  {
    struct
    {
      Byte B;
      Byte G;
      Byte R;
      Byte A;
    };
    TColor32 Color;
    Byte Channels[4];
  };
} TColor32Rec;

typedef struct TColor48Rec
{
  union
  {
    struct
    {
      Word B;
      Word G;
      Word R;
    };
    Word Channels[3];
  };
} TColor48Rec;

typedef struct TColor64Rec
{
  union
  {
    struct
    {
      Word B;
      Word G;
      Word R;
      Word A;

    };
    TColor64 Color;
    Word Channels[4];
  };
} TColor64Rec;

typedef struct TColorFPRec
{
  union
  {
    struct
    {
      float B;
      float G;
      float R;
      float A;
    };
    float Channels[4];
  };
} TColorFPRec;

typedef TColor32Rec * PPalette32;

typedef struct TImageData
{
  int Width;
  int Height;
  TImageFormat Format;
  int Size;
  void * Bits;
  PPalette32 Palette;
} TImageData;

typedef TImageData * PImageData;

typedef struct TPixelFormat
{
  Byte ABitCount, RBitCount, GBitCount, BBitCount;
  LongWord ABitMask, RBitMask, GBitMask, BBitMask;
  Byte AShift, RShift, GShift, BShift;
  Byte ARecDiv, RRecDiv, GRecDiv, BRecDiv;
} TPixelFormat;

typedef TPixelFormat * PPixelFormat;

typedef struct TImageFormatInfo
{
  TImageFormat Format;
  char Name[16];
  Byte BytesPerPixel;
  int PaletteEntries;
  Boolean HasGrayChannel;
  Boolean HasAlphaChannel;
  Boolean IsFloatingPoint;
  Boolean UsePixelFormat;
  Boolean IsRBSwapped;
  Boolean IsIndexed;
  Boolean IsSpecial;
  PPixelFormat PixelFormat;
  Pointer GetPixelsSize; 
  Pointer CheckDimensions; 
  Pointer GetPixel32;    
  Pointer GetPixelFP;  
  Pointer SetPixel32;  
  Pointer SetPixelFP;  
} TImageFormatInfo;

typedef TImageFormatInfo * PImageFormatInfo;

typedef unsigned long TImageDataList;
typedef TImageDataList * PImageDataList;

typedef Pointer TImagingHandle;

typedef enum TResizeFilter
{
  rfNearest  = 0,
  rfBilinear = 1,
  rfBicubic  = 2
} TResizeFilter;

typedef enum TSeekMode
{
  smFromBeginning = 0,
  smFromCurrent   = 1,
  smFromEnd       = 2
} TSeekMode;

typedef TImagingHandle  (ImagingAPI * TOpenReadProc) (char * Source);
typedef TImagingHandle (ImagingAPI * TOpenWriteProc) (char * Source);
typedef void (ImagingAPI * TCloseProc) (TImagingHandle Handle);
typedef Boolean (ImagingAPI * TEofProc) (TImagingHandle Handle);
typedef int (ImagingAPI * TSeekProc) (TImagingHandle Handle, int Offset, TSeekMode Mode);
typedef int (ImagingAPI * TTellProc) (TImagingHandle Handle);
typedef int (ImagingAPI * TReadProc) (TImagingHandle Handle, Pointer Buffer, int Count);
typedef int (ImagingAPI * TWriteProc) (TImagingHandle Handle, Pointer Buffer, int Count);

#ifdef __cplusplus
  }
}
#endif

#pragma pack(pop)

#endif

/*
  Changes/Bug Fixes:

  -- 0.21 -----------------------------------------------------
	- Added ifForce32 to TImageFormat to ensure that this enum has size of 4 bytes. 

*/
