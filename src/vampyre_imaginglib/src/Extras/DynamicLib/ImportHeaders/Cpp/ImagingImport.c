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

#include "ImagingImport.h"
#ifdef __cplusplus
namespace Imaging
{
  extern "C"
  {
#endif

#ifndef NULL
  #define NULL 0
#endif   

/* General Functions */
TImGetVersion ImGetVersion = NULL;
TImInitImage ImInitImage = NULL;
TImNewImage ImNewImage = NULL;
TImTestImage ImTestImage = NULL;
TImFreeImage ImFreeImage = NULL;
TImDetermineFileFormat ImDetermineFileFormat = NULL;
TImDetermineMemoryFormat ImDetermineMemoryFormat = NULL;
TImIsFileFormatSupported ImIsFileFormatSupported = NULL;
TImEnumFileFormats ImEnumFileFormats = NULL;
/* Image List Functions */
TImInitImageList ImInitImageList = NULL;
TImGetImageListSize ImGetImageListSize = NULL;
TImGetImageListElement ImGetImageListElement = NULL;
TImSetImageListSize ImSetImageListSize = NULL;
TImSetImageListElement ImSetImageListElement = NULL;
TImTestImagesInList ImTestImagesInList = NULL;
TImFreeImageList ImFreeImageList = NULL;
/* Loading Functions */
TImLoadImageFromFile ImLoadImageFromFile = NULL;
TImLoadImageFromMemory ImLoadImageFromMemory = NULL;
TImLoadMultiImageFromFile ImLoadMultiImageFromFile = NULL;
TImLoadMultiImageFromMemory ImLoadMultiImageFromMemory = NULL;
/* Saving Functions */
TImSaveImageToFile ImSaveImageToFile = NULL;
TImSaveImageToMemory ImSaveImageToMemory = NULL;
TImSaveMultiImageToFile ImSaveMultiImageToFile = NULL;
TImSaveMultiImageToMemory ImSaveMultiImageToMemory = NULL;
/* Manipulation Functions */
TImCloneImage ImCloneImage = NULL;
TImConvertImage ImConvertImage = NULL;
TImFlipImage ImFlipImage = NULL;
TImMirrorImage ImMirrorImage = NULL;
TImResizeImage ImResizeImage = NULL;
TImSwapChannels ImSwapChannels = NULL;
TImReduceColors ImReduceColors = NULL;
TImGenerateMipMaps ImGenerateMipMaps = NULL;
TImMapImageToPalette ImMapImageToPalette = NULL;
TImSplitImage ImSplitImage = NULL;
TImMakePaletteForImages ImMakePaletteForImages = NULL;
TImRotateImage ImRotateImage = NULL;
/* Drawing/Pixel functions */
TImCopyRect ImCopyRect = NULL;
TImFillRect ImFillRect = NULL;
TImReplaceColor ImReplaceColor = NULL;
TImStretchRect ImStretchRect = NULL;
TImGetPixelDirect ImGetPixelDirect = NULL;
TImSetPixelDirect ImSetPixelDirect = NULL;
TImGetPixel32 ImGetPixel32 = NULL;
TImSetPixel32 ImSetPixel32 = NULL;
TImGetPixelFP ImGetPixelFP = NULL;
TImSetPixelFP ImSetPixelFP = NULL;
/* Palette Functions */
TImNewPalette ImNewPalette;
TImFreePalette ImFreePalette;
TImCopyPalette ImCopyPalette;
TImFindColor ImFindColor = NULL;
TImFillGrayscalePalette ImFillGrayscalePalette = NULL;
TImFillCustomPalette ImFillCustomPalette = NULL;
TImSwapChannelsOfPalette ImSwapChannelsOfPalette = NULL;
/* Options Functions */
TImSetOption ImSetOption = NULL;
TImGetOption ImGetOption = NULL;
TImPopOptions ImPopOptions = NULL;
TImPushOptions ImPushOptions = NULL;
/* Image Format Functions */
TImGetPixelBytes ImGetPixelBytes = NULL;
TImGetImageFormatInfo ImGetImageFormatInfo = NULL;
TImGetPixelsSize ImGetPixelsSize = NULL;
/* IO Functions */
TImSetUserFileIO ImSetUserFileIO = NULL;
TImResetFileIO ImResetFileIO = NULL;

TModuleHandle LibHandle = NULL;

Boolean ImLoadLibrary(void)
{
  LibHandle = DllLoad(LibraryName);
  if (!LibHandle)
    return False;

  /* General Functions */
  ImGetVersion = (TImGetVersion)DllGet(LibHandle, "ImGetVersion");
  ImInitImage = (TImInitImage)DllGet(LibHandle, "ImInitImage");
  ImNewImage = (TImNewImage)DllGet(LibHandle, "ImNewImage");
  ImTestImage = (TImTestImage)DllGet(LibHandle, "ImTestImage");
  ImFreeImage = (TImFreeImage)DllGet(LibHandle, "ImFreeImage");
  ImDetermineFileFormat = (TImDetermineFileFormat)DllGet(LibHandle, "ImDetermineFileFormat");
  ImDetermineMemoryFormat = (TImDetermineMemoryFormat)DllGet(LibHandle, "ImDetermineMemoryFormat");
  ImIsFileFormatSupported = (TImIsFileFormatSupported)DllGet(LibHandle, "ImIsFileFormatSupported");  
  ImEnumFileFormats = (TImEnumFileFormats)DllGet(LibHandle, "ImEnumFileFormats");
  /* Image List Functions */
  ImInitImageList = (TImInitImageList)DllGet(LibHandle, "ImInitImageList");
  ImGetImageListSize = (TImGetImageListSize)DllGet(LibHandle, "ImGetImageListSize");
  ImGetImageListElement = (TImGetImageListElement)DllGet(LibHandle, "ImGetImageListElement");
  ImSetImageListSize = (TImSetImageListSize)DllGet(LibHandle, "ImSetImageListSize");
  ImSetImageListElement = (TImSetImageListElement)DllGet(LibHandle, "ImSetImageListElement");
  ImTestImagesInList = (TImTestImagesInList)DllGet(LibHandle, "ImTestImagesInList");
  ImFreeImageList = (TImFreeImageList)DllGet(LibHandle, "ImFreeImageList");
  /* Loading Functions */
  ImLoadImageFromFile = (TImLoadImageFromFile)DllGet(LibHandle, "ImLoadImageFromFile");
  ImLoadImageFromMemory = (TImLoadImageFromMemory)DllGet(LibHandle, "ImLoadImageFromMemory");
  ImLoadMultiImageFromFile = (TImLoadMultiImageFromFile)DllGet(LibHandle, "ImLoadMultiImageFromFile");
  ImLoadMultiImageFromMemory = (TImLoadMultiImageFromMemory)DllGet(LibHandle, "ImLoadMultiImageFromMemory");
  /* Saving Functions */
  ImSaveImageToFile = (TImSaveImageToFile)DllGet(LibHandle, "ImSaveImageToFile");
  ImSaveImageToMemory = (TImSaveImageToMemory)DllGet(LibHandle, "ImSaveImageToMemory");
  ImSaveMultiImageToFile = (TImSaveMultiImageToFile)DllGet(LibHandle, "ImSaveMultiImageToFile");
  ImSaveMultiImageToMemory = (TImSaveMultiImageToMemory)DllGet(LibHandle, "ImSaveMultiImageToMemory");
  /* Manipulation Functions */
  ImCloneImage = (TImCloneImage)DllGet(LibHandle, "ImCloneImage");
  ImConvertImage = (TImConvertImage)DllGet(LibHandle, "ImConvertImage");
  ImFlipImage = (TImFlipImage)DllGet(LibHandle, "ImFlipImage");
  ImMirrorImage = (TImMirrorImage)DllGet(LibHandle, "ImMirrorImage");
  ImResizeImage = (TImResizeImage)DllGet(LibHandle, "ImResizeImage");
  ImSwapChannels = (TImSwapChannels)DllGet(LibHandle, "ImSwapChannels");
  ImReduceColors = (TImReduceColors)DllGet(LibHandle, "ImReduceColors");
  ImGenerateMipMaps = (TImGenerateMipMaps)DllGet(LibHandle, "ImGenerateMipMaps");
  ImMapImageToPalette = (TImMapImageToPalette)DllGet(LibHandle, "ImMapImageToPalette");
  ImSplitImage = (TImSplitImage)DllGet(LibHandle, "ImSplitImage");
  ImMakePaletteForImages = (TImMakePaletteForImages)DllGet(LibHandle, "ImMakePaletteForImages");
  ImRotateImage = (TImRotateImage)DllGet(LibHandle, "ImRotateImage");
  /* Drawing/Pixel functions */
  ImCopyRect = (TImCopyRect)DllGet(LibHandle, "ImCopyRect");
  ImFillRect = (TImFillRect)DllGet(LibHandle, "ImFillRect");
  ImReplaceColor = (TImReplaceColor)DllGet(LibHandle, "ImReplaceColor");
  ImStretchRect = (TImStretchRect)DllGet(LibHandle, "ImStretchRect");
  ImGetPixelDirect = (TImGetPixelDirect)DllGet(LibHandle, "ImGetPixelDirect");
  ImSetPixelDirect = (TImSetPixelDirect)DllGet(LibHandle, "ImSetPixelDirect");
  ImGetPixel32 = (TImGetPixel32)DllGet(LibHandle, "ImGetPixel32");
  ImSetPixel32 = (TImSetPixel32)DllGet(LibHandle, "ImSetPixel32");
  ImGetPixelFP = (TImGetPixelFP)DllGet(LibHandle, "ImGetPixelFP");
  ImSetPixelFP = (TImSetPixelFP)DllGet(LibHandle, "ImSetPixelFP");
  /* Palette Functions */
  ImNewPalette = (TImNewPalette)DllGet(LibHandle, "ImNewPalette");
  ImFreePalette = (TImFreePalette)DllGet(LibHandle, "ImFreePalette");
  ImCopyPalette = (TImCopyPalette)DllGet(LibHandle, "ImCopyPalette");
  ImFindColor = (TImFindColor)DllGet(LibHandle, "ImFindColor");
  ImFillGrayscalePalette = (TImFillGrayscalePalette)DllGet(LibHandle, "ImFillGrayscalePalette");
  ImFillCustomPalette = (TImFillCustomPalette)DllGet(LibHandle, "ImFillCustomPalette");
  ImSwapChannelsOfPalette = (TImSwapChannelsOfPalette)DllGet(LibHandle, "ImSwapChannelsOfPalette");
  /* Options Functions */
  ImSetOption = (TImSetOption)DllGet(LibHandle, "ImSetOption");
  ImGetOption = (TImGetOption)DllGet(LibHandle, "ImGetOption");
  ImPopOptions = (TImPopOptions)DllGet(LibHandle, "ImPopOptions");
  ImPushOptions = (TImPushOptions)DllGet(LibHandle, "ImPushOptions");
  /* Image Format Functions */
  ImGetPixelBytes = (TImGetPixelBytes)DllGet(LibHandle, "ImGetPixelBytes");
  ImGetImageFormatInfo = (TImGetImageFormatInfo)DllGet(LibHandle, "ImGetImageFormatInfo");
  ImGetPixelsSize = (TImGetPixelsSize)DllGet(LibHandle, "ImGetPixelsSize");	
  /* IO Functions */
  ImSetUserFileIO = (TImSetUserFileIO)DllGet(LibHandle, "ImSetUserFileIO");
  ImResetFileIO = (TImResetFileIO)DllGet(LibHandle, "ImResetFileIO");

  return True;
}

Boolean ImFreeLibrary(void)
{
  if (DllFree(LibHandle))
    return True;
  else
    return False;
}

#ifdef __cplusplus
  }
}
#endif

/*
  Changes/Bug Fixes:

  -- 0.13 -----------------------------------------------------
    - file created with instances of extern variables from ImagingImport.h
	  and ImLoadLibrary/ImFreeLibrary functions

*/
