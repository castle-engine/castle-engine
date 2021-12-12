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

{ This unit contains Pascal interface to Imaging library which is
  compiled into the dynamic link library.}
unit ImagingImport;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes;

const
{$IFDEF MSWINDOWS}
  LibraryName = 'VampyreImaging.dll';
{$ENDIF}
{$IFDEF LINUX}
  LibraryName = 'libVampyreImaging.so';
{$ENDIF}

procedure ImGetVersion(var Major, Minor, Patch: LongInt); cdecl; external LibraryName;
procedure ImInitImage(var Image: TImageData); cdecl; external LibraryName;
function ImNewImage(Width, Height: LongInt; Format: TImageFormat; var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImTestImage(var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImFreeImage(var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImDetermineFileFormat(FileName, Ext: PAnsiChar): Boolean; cdecl; external LibraryName;
function ImDetermineMemoryFormat(Data: Pointer; Size: LongInt; Ext: PAnsiChar): Boolean; cdecl; external LibraryName;
function ImIsFileFormatSupported(FileName: PAnsiChar): Boolean; cdecl; external LibraryName;
function ImEnumFileFormats(var Index: LongInt; Name, DefaultExt, Masks: PAnsiChar; var CanSave, IsMultiImageFormat: Boolean): Boolean; cdecl; external LibraryName;

function ImInitImageList(Size: LongInt; var ImageList: TImageDataList): Boolean; cdecl; external LibraryName;
function ImGetImageListSize(ImageList: TImageDataList): LongInt; cdecl; external LibraryName;
function ImGetImageListElement(ImageList: TImageDataList; Index: LongInt; var OutImage: TImageData): Boolean; cdecl; external LibraryName;
function ImSetImageListSize(ImageList: TImageDataList; NewSize: LongInt): Boolean; cdecl; external LibraryName;
function ImSetImageListElement(ImageList: TImageDataList; Index: LongInt; const InImage: TImageData): Boolean; cdecl; external LibraryName;
function ImTestImagesInList(ImageList: TImageDataList): Boolean; cdecl; external LibraryName;
function ImFreeImageList(var ImageList: TImageDataList): Boolean; cdecl; external LibraryName;

function ImLoadImageFromFile(FileName: PAnsiChar; var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImLoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImLoadMultiImageFromFile(FileName: PAnsiChar; var ImageList: TImageDataList): Boolean; cdecl; external LibraryName;
function ImLoadMultiImageFromMemory(Data: Pointer; Size: LongInt; var ImageList: TImageDataList): Boolean; cdecl; external LibraryName;

function ImSaveImageToFile(FileName: PAnsiChar; const Image: TImageData): Boolean; cdecl; external LibraryName;
function ImSaveImageToMemory(Ext: PAnsiChar; Data: Pointer; var Size: LongInt; const Image: TImageData): Boolean; cdecl; external LibraryName;
function ImSaveMultiImageToFile(FileName: PAnsiChar; ImageList: TImageDataList): Boolean; cdecl; external LibraryName;
function ImSaveMultiImageToMemory(Ext: PAnsiChar; Data: Pointer; Size: PLongInt; ImageList: TImageDataList): Boolean; cdecl; external LibraryName;

function ImCloneImage(const Image: TImageData; var Clone: TImageData): Boolean; cdecl; external LibraryName;
function ImConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean; cdecl; external LibraryName;
function ImFlipImage(var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImMirrorImage(var Image: TImageData): Boolean; cdecl; external LibraryName;
function ImResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt; Filter: TResizeFilter): Boolean; cdecl; external LibraryName;
function ImSwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean; cdecl; external LibraryName;
function ImReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean; cdecl; external LibraryName;
function ImGenerateMipMaps(const Image: TImageData; Levels: LongInt; var MipMaps: TImageDataList): Boolean; cdecl; external LibraryName;
function ImMapImageToPalette(var Image: TImageData; Pal: PPalette32; Entries: LongInt): Boolean; cdecl; external LibraryName;
function ImSplitImage(var Image: TImageData; var Chunks: TImageDataList; ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt; PreserveSize: Boolean; Fill: Pointer): Boolean; cdecl; external LibraryName;
function ImMakePaletteForImages(Images: TImageDataList; Pal: PPalette32; MaxColors: LongInt; ConvertImages: Boolean): Boolean; cdecl; external LibraryName;
function ImRotateImage(var Image: TImageData; Angle: Single): Boolean; cdecl; external LibraryName;

function ImCopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt; var DstImage: TImageData; DstX, DstY: LongInt): Boolean; cdecl; external LibraryName;
function ImFillRect(var Image: TImageData; X, Y, Width, Height: LongInt; Fill: Pointer): Boolean; cdecl; external LibraryName;
function ImReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt; OldPixel, NewPixel: Pointer): Boolean; cdecl; external LibraryName;
function ImStretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth, SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth, DstHeight: LongInt; Filter: TResizeFilter): Boolean; cdecl; external LibraryName;
procedure ImGetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer); cdecl; external LibraryName;
procedure ImSetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer); cdecl; external LibraryName;
function ImGetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec; cdecl; external LibraryName;
procedure ImSetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec); cdecl; external LibraryName;
function ImGetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec; cdecl; external LibraryName;
procedure ImSetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); cdecl; external LibraryName;

function ImNewPalette(Entries: LongInt; var Pal: PPalette32): Boolean; cdecl; external LibraryName;
function ImFreePalette(var Pal: PPalette32): Boolean; cdecl; external LibraryName;
function ImCopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean; cdecl; external LibraryName;
function ImFindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32): LongInt; cdecl; external LibraryName;
function ImFillGrayscalePalette(Pal: PPalette32; Entries: LongInt): Boolean; cdecl; external LibraryName;
function ImFillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits, BBits: Byte; Alpha: Byte): Boolean; cdecl; external LibraryName;
function ImSwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel, DstChannel: LongInt): Boolean; cdecl; external LibraryName;

function ImSetOption(OptionId, Value: LongInt): Boolean; cdecl; external LibraryName;
function ImGetOption(OptionId: LongInt): LongInt; cdecl; external LibraryName;
function ImPushOptions: Boolean; cdecl; external LibraryName;
function ImPopOptions: Boolean; cdecl; external LibraryName;

function ImGetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean; cdecl; external LibraryName;
function ImGetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; cdecl; external LibraryName;

procedure ImSetUserFileIO(OpenReadProc: TOpenReadProc; OpenWriteProc:
  TOpenWriteProc; CloseProc: TCloseProc; EofProc: TEofProc; SeekProc: TSeekProc;
  TellProc: TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc); cdecl; external LibraryName;
procedure ImResetFileIO; cdecl; external LibraryName;

implementation

end.

