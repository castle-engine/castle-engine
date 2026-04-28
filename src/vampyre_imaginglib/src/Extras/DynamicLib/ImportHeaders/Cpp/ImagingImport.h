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

#ifndef IMAGINGIMPORT_H
#define IMAGINGIMPORT_H
#include "ImagingTypes.h"

#ifdef __cplusplus
namespace Imaging
{
  extern "C"
  {
#endif

#if defined(__WIN32__) || defined(_WIN32)
  #include <windows.h>
  typedef HMODULE TModuleHandle;
  #define LibraryName      "VampyreImaging.dll"
  #define DllLoad(A)       LoadLibrary(A)
  #define DllGet(A, B)     GetProcAddress(A, B)
  #define DllFree(A)       FreeLibrary(A)
#elif defined (linux) || defined(__linux__)
  #include <dlfcn.h>
  typedef void * TModuleHandle;
  #define LibraryName      "libVampyreImaging.so"
  #define DllLoad(A)       dlopen(A, RTLD_LAZY)
  #define DllGet(A, B)     dlsym(A, B)
  #define DllFree(A)       dlclose(A)
#endif

/* Loads Imaging dynamic library, must be called before any other function. */
extern Boolean ImLoadLibrary(void);
/* Frees Imaging library. */
extern Boolean ImFreeLibrary(void);

/* General Functions */

typedef void (ImagingAPI * TImGetVersion)(int * Major, int * Minor, int * Patch);
typedef void (ImagingAPI * TImInitImage)(PImageData Image);
typedef Boolean (ImagingAPI * TImNewImage)(int Width, int Height, TImageFormat Format, PImageData Image);
typedef Boolean (ImagingAPI * TImTestImage)(PImageData Image);
typedef Boolean (ImagingAPI * TImFreeImage)(PImageData Image);
typedef Boolean (ImagingAPI * TImDetermineFileFormat)(const char * FileName, char * Ext);
typedef Boolean (ImagingAPI * TImDetermineMemoryFormat)(const void * Data, int Size, char * Ext);
typedef Boolean (ImagingAPI * TImIsFileFormatSupported)(const char * FileName);
typedef Boolean (ImagingAPI * TImEnumFileFormats)(int * Index, char * Name, char * DefaultExt, char * Masks, Boolean * CanSave, Boolean * IsMultiImageFormat);

extern TImGetVersion ImGetVersion;
extern TImInitImage ImInitImage;
extern TImNewImage ImNewImage;
extern TImTestImage ImTestImage;
extern TImFreeImage ImFreeImage;
extern TImDetermineFileFormat ImDetermineFileFormat;
extern TImDetermineMemoryFormat ImDetermineMemoryFormat;
extern TImIsFileFormatSupported ImIsFileFormatSupported;
extern TImEnumFileFormats ImEnumFileFormats;

/* Image List Functions */

typedef Boolean (ImagingAPI * TImInitImageList)(int Size, PImageDataList ImageList);
typedef int (ImagingAPI * TImGetImageListSize)(PImageDataList ImageList);
typedef Boolean (ImagingAPI * TImGetImageListElement)(TImageDataList ImageList, int Index, PImageData OutImage);
typedef Boolean (ImagingAPI * TImSetImageListSize)(TImageDataList ImageList, int NewSize);
typedef Boolean (ImagingAPI * TImSetImageListElement)(TImageDataList ImageList, int Index, PImageData InImage);
typedef Boolean (ImagingAPI * TImTestImagesInList)(TImageDataList ImageList);
typedef Boolean (ImagingAPI * TImFreeImageList)(PImageDataList ImageList);

extern TImInitImageList ImInitImageList;
extern TImGetImageListSize ImGetImageListSize;
extern TImGetImageListElement ImGetImageListElement;
extern TImSetImageListSize ImSetImageListSize;
extern TImSetImageListElement ImSetImageListElement;
extern TImTestImagesInList ImTestImagesInList;
extern TImFreeImageList ImFreeImageList;

/* Loading Functions */

typedef Boolean (ImagingAPI * TImLoadImageFromFile)(const char * FileName, PImageData Image);
typedef Boolean (ImagingAPI * TImLoadImageFromMemory)(const void * Data, int Size, PImageData Image);
typedef Boolean (ImagingAPI * TImLoadMultiImageFromFile)(const char * FileName, PImageDataList ImageList);
typedef Boolean (ImagingAPI * TImLoadMultiImageFromMemory)(const void * Data, int Size, PImageDataList ImageList);

extern TImLoadImageFromFile ImLoadImageFromFile;
extern TImLoadImageFromMemory ImLoadImageFromMemory;
extern TImLoadMultiImageFromFile ImLoadMultiImageFromFile;
extern TImLoadMultiImageFromMemory ImLoadMultiImageFromMemory;

/* Saving Functions */

typedef Boolean (ImagingAPI * TImSaveImageToFile)(const char * FileName, PImageData Image);
typedef Boolean (ImagingAPI * TImSaveImageToMemory)(const char * Ext, void * Data, int * Size, PImageData Image);
typedef Boolean (ImagingAPI * TImSaveMultiImageToFile)(const char * FileName, TImageDataList ImageList);
typedef Boolean (ImagingAPI * TImSaveMultiImageToMemory)(const char * Ext, void * Data, int * Size, TImageDataList ImageList);

extern TImSaveImageToFile ImSaveImageToFile;
extern TImSaveImageToMemory ImSaveImageToMemory;
extern TImSaveMultiImageToFile ImSaveMultiImageToFile;
extern TImSaveMultiImageToMemory ImSaveMultiImageToMemory;

/* Manipulation Functions */

typedef Boolean (ImagingAPI * TImCloneImage)(PImageData Image, PImageData Clone);
typedef Boolean (ImagingAPI * TImConvertImage)(PImageData Image, TImageFormat DestFormat);
typedef Boolean (ImagingAPI * TImFlipImage)(PImageData Image);
typedef Boolean (ImagingAPI * TImMirrorImage)(PImageData Image);
typedef Boolean (ImagingAPI * TImResizeImage)(PImageData Image, int NewWidth, int NewHeight, TResizeFilter Filter);
typedef Boolean (ImagingAPI * TImSwapChannels)(PImageData Image, int SrcChannel, int DstChannel);
typedef Boolean (ImagingAPI * TImReduceColors)(PImageData Image, int MaxColors);
typedef Boolean (ImagingAPI * TImGenerateMipMaps)(PImageData Image, int Levels, PImageDataList MipMaps);
typedef Boolean (ImagingAPI * TImMapImageToPalette)(PImageData Image, PPalette32 Pal, int Entries);
typedef Boolean (ImagingAPI * TImSplitImage)(PImageData Image, PImageDataList Chunks, int ChunkWidth, int ChunkHeight, int * XChunks, int * YChunks, Boolean PreserveSize, const void * Fill);
typedef Boolean (ImagingAPI * TImMakePaletteForImages)(TImageDataList Images, PPalette32 Pal, int MaxColors, Boolean ConvertImages);
typedef Boolean (ImagingAPI * TImRotateImage)(PImageData Image, float Angle);

extern TImCloneImage ImCloneImage;
extern TImConvertImage ImConvertImage;
extern TImFlipImage ImFlipImage;
extern TImMirrorImage ImMirrorImage;
extern TImResizeImage ImResizeImage;
extern TImSwapChannels ImSwapChannels;
extern TImReduceColors ImReduceColors;
extern TImGenerateMipMaps ImGenerateMipMaps;
extern TImMapImageToPalette ImMapImageToPalette;
extern TImSplitImage ImSplitImage;
extern TImMakePaletteForImages ImMakePaletteForImages;
extern TImRotateImage ImRotateImage;

/* Drawing/Pixel functions */

typedef Boolean (ImagingAPI * TImCopyRect)(PImageData SrcImage, int SrcX, int SrcY, int Width, int Height, PImageData DstImage, int DstX, int DstY);
typedef Boolean (ImagingAPI * TImFillRect)(PImageData Image, int  X, int Y, int Width, int Height, const void * Fill);
typedef Boolean (ImagingAPI * TImReplaceColor)(PImageData Image, int  X, int Y, int Width, int Height, const void * OldPixel, const void * NewPixel);
typedef Boolean (ImagingAPI * TImStretchRect)(PImageData SrcImage, int SrcX, int SrcY, int SrcWidth, int SrcHeight, PImageData DstImage, int DstX, int DstY, int DstWidth, int DstHeight, TResizeFilter Filter);
typedef void (ImagingAPI * TImGetPixelDirect)(PImageData Image, int X, int Y, Pointer Pixel);
typedef void (ImagingAPI * TImSetPixelDirect)(PImageData Image, int X, int Y, Pointer Pixel);
typedef TColor32Rec (ImagingAPI * TImGetPixel32)(PImageData Image, int X, int Y);
typedef void (ImagingAPI * TImSetPixel32)(PImageData Image, int X, int Y, TColor32Rec Color);
typedef TColorFPRec (ImagingAPI * TImGetPixelFP)(PImageData Image, int X, int Y);
typedef void (ImagingAPI * TImSetPixelFP)(PImageData Image, int X, int Y, TColorFPRec Color);

extern TImCopyRect ImCopyRect;
extern TImFillRect ImFillRect;
extern TImReplaceColor ImReplaceColor;
extern TImStretchRect ImStretchRect;
extern TImGetPixelDirect ImGetPixelDirect;
extern TImSetPixelDirect ImSetPixelDirect;
extern TImGetPixel32 ImGetPixel32;
extern TImSetPixel32 ImSetPixel32;
extern TImGetPixelFP ImGetPixelFP;
extern TImSetPixelFP ImSetPixelFP;

/* Palette Functions */

typedef Boolean (ImagingAPI * TImNewPalette)(int Entries, PPalette32 * Pal);
typedef Boolean (ImagingAPI * TImFreePalette)(PPalette32 * Pal);
typedef Boolean (ImagingAPI * TImCopyPalette)(PPalette32 SrcPal, PPalette32 DstPal, int SrcIdx, int DstIdx, int Count);
typedef int (ImagingAPI * TImFindColor)(PPalette32 Pal, int Entries, TColor32 Color);
typedef Boolean (ImagingAPI * TImFillGrayscalePalette)(PPalette32 Pal, int Entries);
typedef Boolean (ImagingAPI * TImFillCustomPalette)(PPalette32 Pal, int Entries, Byte RBits, Byte GBits, Byte BBits, Byte Alpha);
typedef Boolean (ImagingAPI * TImSwapChannelsOfPalette)(PPalette32 Pal, int Entries, int SrcChannel, int DstChannel);

extern TImNewPalette ImNewPalette;
extern TImFreePalette ImFreePalette;
extern TImCopyPalette ImCopyPalette;
extern TImFindColor ImFindColor;
extern TImFillGrayscalePalette ImFillGrayscalePalette;
extern TImFillCustomPalette ImFillCustomPalette;
extern TImSwapChannelsOfPalette ImSwapChannelsOfPalette;

/* Options Functions */

typedef Boolean (ImagingAPI * TImSetOption)(int OptionId, int Value);
typedef int (ImagingAPI * TImGetOption)(int OptionId);
typedef Boolean (ImagingAPI * TImPushOptions)(void);
typedef Boolean (ImagingAPI * TImPopOptions)(void);

extern TImSetOption ImSetOption;
extern TImGetOption ImGetOption;
extern TImPopOptions ImPopOptions;
extern TImPushOptions ImPushOptions;

/* Image Format Functions */

typedef int (ImagingAPI * TImGetPixelBytes)(TImageFormat Format);
typedef Boolean (ImagingAPI * TImGetImageFormatInfo)(TImageFormat Format, PImageFormatInfo Info);
typedef int (ImagingAPI * TImGetPixelsSize)(TImageFormat Format, int Width, int Height);

extern TImGetPixelBytes ImGetPixelBytes;
extern TImGetImageFormatInfo ImGetImageFormatInfo;
extern TImGetPixelsSize ImGetPixelsSize;

/* IO Functions */

typedef Boolean (ImagingAPI * TImSetUserFileIO)(TOpenReadProc OpenReadProc, TOpenWriteProc OpenWriteProc, TCloseProc CloseProc, TEofProc EofProc, TSeekProc SeekProc, TTellProc TellProc, TReadProc ReadProc, TWriteProc WriteProc);
typedef Boolean (ImagingAPI * TImResetFileIO)(void);

extern TImSetUserFileIO ImSetUserFileIO;
extern TImResetFileIO ImResetFileIO;

#ifdef __cplusplus
  }
}
#endif

#endif

/*
  Changes/Bug Fixes:

  -- 0.21 -----------------------------------------------------
    - Updated to current DLL version.

  -- 0.15 -----------------------------------------------------
    - changed some parameter declarations in headers of some functions 
	  because of changes in Imaging dll, mainly ImageDataList stuff

*/
