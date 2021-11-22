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

{ This function contains functions exported from Imaging dynamic link library.
  All string are exported as PChars and all var parameters are exported
  as pointers. All posible exceptions getting out of dll are catched.}
unit ImagingExport;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, 
  Imaging;

{ Returns version of Imaging library. }
procedure ImGetVersion(var Major, Minor, Patch: LongInt); cdecl;
{ Look at InitImage for details.}
procedure ImInitImage(var Image: TImageData); cdecl;
{ Look at NewImage for details.}
function ImNewImage(Width, Height: LongInt; Format: TImageFormat;
  var Image: TImageData): Boolean; cdecl;
{ Look at TestImage for details.}
function ImTestImage(var Image: TImageData): Boolean; cdecl;
{ Look at FreeImage for details.}
function ImFreeImage(var Image: TImageData): Boolean; cdecl;
{ Look at DetermineFileFormat for details. Ext should have enough space for
  result file extension.}
function ImDetermineFileFormat(FileName, Ext: PAnsiChar): Boolean; cdecl;
{ Look at DetermineMemoryFormat for details. Ext should have enough space for
  result file extension.}
function ImDetermineMemoryFormat(Data: Pointer; Size: LongInt; Ext: PAnsiChar): Boolean; cdecl;
{ Look at IsFileFormatSupported for details.}
function ImIsFileFormatSupported(FileName: PAnsiChar): Boolean; cdecl;
{ Look at EnumFileFormats for details.}
function ImEnumFileFormats(var Index: LongInt; Name, DefaultExt, Masks: PAnsiChar;
  var CanSave, IsMultiImageFormat: Boolean): Boolean; cdecl;

{ Inits image list.}
function ImInitImageList(Size: LongInt; var ImageList: TImageDataList): Boolean; cdecl;
{ Returns size of image list.}
function ImGetImageListSize(ImageList: TImageDataList): LongInt; cdecl;
{ Returns image list's element at given index. Output image is not cloned it's
  Bits point to Bits in list => do not free OutImage.}
function ImGetImageListElement(ImageList: TImageDataList; Index: LongInt;
  var OutImage: TImageData): Boolean; cdecl;
{ Sets size of image list.}
function ImSetImageListSize(ImageList: TImageDataList; NewSize: LongInt): Boolean; cdecl;
{ Sets image list element at given index. Input image is not cloned - image in
  list will point to InImage's Bits.}
function ImSetImageListElement(ImageList: TImageDataList; Index: LongInt;
  const InImage: TImageData): Boolean; cdecl;
{ Returns True if all images in list pass ImTestImage test. }
function ImTestImagesInList(ImageList: TImageDataList): Boolean; cdecl;
{ Frees image list and all images in it.}
function ImFreeImageList(var ImageList: TImageDataList): Boolean; cdecl;

{ Look at LoadImageFromFile for details.}
function ImLoadImageFromFile(FileName: PAnsiChar; var Image: TImageData): Boolean; cdecl;
{ Look at LoadImageFromMemory for details.}
function ImLoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean; cdecl;
{ Look at LoadMultiImageFromFile for details.}
function ImLoadMultiImageFromFile(FileName: PAnsiChar; var ImageList: TImageDataList): Boolean; cdecl;
{ Look at LoadMultiImageFromMemory for details.}
function ImLoadMultiImageFromMemory(Data: Pointer; Size: LongInt;
  var ImageList: TImageDataList): Boolean; cdecl;

{ Look at SaveImageToFile for details.}
function ImSaveImageToFile(FileName: PAnsiChar; const Image: TImageData): Boolean; cdecl;
{ Look at SaveImageToMemory for details.}
function ImSaveImageToMemory(Ext: PAnsiChar; Data: Pointer; var Size: LongInt;
  const Image: TImageData): Boolean; cdecl;
{ Look at SaveMultiImageToFile for details.}
function ImSaveMultiImageToFile(FileName: PAnsiChar; ImageList: TImageDataList): Boolean; cdecl;
{ Look at SaveMultiImageToMemory for details.}
function ImSaveMultiImageToMemory(Ext: PAnsiChar; Data: Pointer; Size: PLongInt;
  ImageList: TImageDataList): Boolean; cdecl;

{ Look at CloneImage for details.}
function ImCloneImage(const Image: TImageData; var Clone: TImageData): Boolean; cdecl;
{ Look at ConvertImage for details.}
function ImConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean; cdecl;
{ Look at FlipImage for details.}
function ImFlipImage(var Image: TImageData): Boolean; cdecl;
{ Look at MirrorImage for details.}
function ImMirrorImage(var Image: TImageData): Boolean; cdecl;
{ Look at ResizeImage for details.}
function ImResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt;
  Filter: TResizeFilter): Boolean; cdecl;
{ Look at SwapChannels for details.}
function ImSwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean; cdecl;
{ Look at ReduceColors for details.}
function ImReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean; cdecl;
{ Look at GenerateMipMaps for details.}
function ImGenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TImageDataList): Boolean; cdecl;
{ Look at MapImageToPalette for details.}
function ImMapImageToPalette(var Image: TImageData; Pal: PPalette32;
  Entries: LongInt): Boolean; cdecl;
{ Look at SplitImage for details.}
function ImSplitImage(var Image: TImageData; var Chunks: TImageDataList;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: Pointer): Boolean; cdecl;
{ Look at MakePaletteForImages for details.}
function ImMakePaletteForImages(Images: TImageDataList; Pal: PPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean; cdecl;
{ Look at RotateImage for details.}
function ImRotateImage(var Image: TImageData; Angle: Single): Boolean; cdecl;

{ Look at CopyRect for details.}
function ImCopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean; cdecl;
{ Look at FillRect for details.}
function ImFillRect(var Image: TImageData; X, Y, Width, Height: LongInt;
  Fill: Pointer): Boolean; cdecl;
{ Look at ReplaceColor for details.}
function ImReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldPixel, NewPixel: Pointer): Boolean; cdecl;
{ Look at StretchRect for details.}
function ImStretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean; cdecl;
{ Look at GetPixelDirect for details.}
procedure ImGetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer); cdecl;
{ Look at SetPixelDirect for details.}
procedure ImSetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer); cdecl;
{ Look at GetPixel32 for details.}
function ImGetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec; cdecl;
{ Look at SetPixel32 for details.}
procedure ImSetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec); cdecl;
{ Look at GetPixelFP for details.}
function ImGetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec; cdecl;
{ Look at SetPixelFP for details.}
procedure ImSetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); cdecl;

{ Look at NewPalette for details.}
function ImNewPalette(Entries: LongInt; var Pal: PPalette32): Boolean; cdecl;
{ Look at FreePalette for details.}
function ImFreePalette(var Pal: PPalette32): Boolean; cdecl;
{ Look at CopyPalette for details.}
function ImCopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean; cdecl;
{ Look at FindColor for details.}
function ImFindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32): LongInt; cdecl;
{ Look at FillGrayscalePalette for details.}
function ImFillGrayscalePalette(Pal: PPalette32; Entries: LongInt): Boolean; cdecl;
{ Look at FillCustomPalette for details.}
function ImFillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte): Boolean; cdecl;
{ Look at SwapChannelsOfPalette for details.}
function ImSwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel,
  DstChannel: LongInt): Boolean; cdecl;

{ Look at SetOption for details.}
function ImSetOption(OptionId, Value: LongInt): Boolean; cdecl;
{ Look at GetOption for details.}
function ImGetOption(OptionId: LongInt): LongInt; cdecl;
{ Look at PushOptions for details.}
function ImPushOptions: Boolean; cdecl;
{ Look at PopOptions for details.}
function ImPopOptions: Boolean; cdecl;

{ Look at GetImageFormatInfo for details.}
function ImGetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean; cdecl;
{ Look at GetPixelsSize for details.}
function ImGetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt; cdecl;

{ Look at SetUserFileIO for details.}
procedure ImSetUserFileIO(OpenProc: TOpenProc; CloseProc: TCloseProc; EofProc: TEofProc;
  SeekProc: TSeekProc; TellProc: TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc); cdecl;
{ Look at ResetFileIO for details.}
procedure ImResetFileIO; cdecl;

{ These are only for documentation generation reasons.}
{ Loads Imaging functions from dll/so library.}
function ImLoadLibrary: Boolean;
{ Frees Imaging functions loaded from dll/so and releases library.}
function ImFreeLibrary: Boolean;

implementation

uses
  SysUtils,
  ImagingUtility;

function ImLoadLibrary: Boolean; begin Result := True; end;
function ImFreeLibrary: Boolean; begin Result := True; end;

type
  TInternalList = record
    List: TDynImageDataArray;
  end;
  PInternalList = ^TInternalList;

procedure ImGetVersion(var Major, Minor, Patch: LongInt);
begin
  Major := ImagingVersionMajor;
  Minor := ImagingVersionMinor;
  Patch := 0;
end;

procedure ImInitImage(var Image: TImageData);
begin
  try
    Imaging.InitImage(Image);
  except
  end;
end;

function ImNewImage(Width, Height: LongInt; Format: TImageFormat;
  var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.NewImage(Width, Height, Format, Image);
  except
    Result := False;
  end;
end;

function ImTestImage(var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.TestImage(Image);
  except
    Result := False;
  end;
end;

function ImFreeImage(var Image: TImageData): Boolean;
begin
  try
    Imaging.FreeImage(Image);
    Result := True;
  except
    Result := False;
  end;
end;

function ImDetermineFileFormat(FileName, Ext: PAnsiChar): Boolean;
var
  S: string;
begin
  try
    S := Imaging.DetermineFileFormat(FileName);
    Result := S <> '';
    StrCopy(Ext, PAnsiChar(AnsiString(S)));
  except
    Result := False;
  end;
end;

function ImDetermineMemoryFormat(Data: Pointer; Size: LongInt; Ext: PAnsiChar): Boolean;
var
  S: string;
begin
  try
    S := Imaging.DetermineMemoryFormat(Data, Size);
    Result := S <> '';
    StrCopy(Ext, PAnsiChar(AnsiString(S)));
  except
    Result := False;
  end;
end;

function ImIsFileFormatSupported(FileName: PAnsiChar): Boolean;
begin
  try
    Result := Imaging.IsFileFormatSupported(FileName);
  except
    Result := False;
  end;
end;

function ImEnumFileFormats(var Index: LongInt; Name, DefaultExt, Masks: PAnsiChar;
  var CanSave, IsMultiImageFormat: Boolean): Boolean;
var
  StrName, StrDefaultExt, StrMasks: string;
begin
  try
    Result := Imaging.EnumFileFormats(Index, StrName, StrDefaultExt, StrMasks, CanSave,
      IsMultiImageFormat);
    StrCopy(Name, PAnsiChar(AnsiString(StrName)));
    StrCopy(DefaultExt, PAnsiChar(AnsiString(StrDefaultExt)));
    StrCopy(Masks, PAnsiChar(AnsiString(StrMasks)));
  except
    Result := False;
  end;
end;

function ImInitImageList(Size: LongInt; var ImageList: TImageDataList): Boolean;
var
  Int: PInternalList;
begin
  try
    try
      ImFreeImageList(ImageList);
    except
    end;
    New(Int);
    SetLength(Int.List, Size);
    ImageList := TImageDataList(Int);
    Result := True;
  except
    Result := False;
    ImageList := nil;
  end;
end;

function ImGetImageListSize(ImageList: TImageDataList): LongInt;
begin
  try
    Result := Length(PInternalList(ImageList).List);
  except
    Result := -1;
  end;
end;

function ImGetImageListElement(ImageList: TImageDataList; Index: LongInt;
  var OutImage: TImageData): Boolean;
begin
  try
    Index := ClampInt(Index, 0, Length(PInternalList(ImageList).List) - 1);
    ImCloneImage(PInternalList(ImageList).List[Index], OutImage);
    Result := True;
  except
    Result := False;
  end;
end;

function ImSetImageListSize(ImageList: TImageDataList; NewSize: LongInt):
  Boolean;
var
  I, OldSize: LongInt;
begin
  try
    OldSize := Length(PInternalList(ImageList).List);
    if NewSize < OldSize then
      for I := NewSize to OldSize - 1 do
        Imaging.FreeImage(PInternalList(ImageList).List[I]);
    SetLength(PInternalList(ImageList).List, NewSize);
    Result := True;
  except
    Result := False;
  end;
end;

function ImSetImageListElement(ImageList: TImageDataList; Index: LongInt;
  const InImage: TImageData): Boolean;
begin
  try
    Index := ClampInt(Index, 0, Length(PInternalList(ImageList).List) - 1);
    ImCloneImage(InImage, PInternalList(ImageList).List[Index]);
    Result := True;
  except
    Result := False;
  end;
end;

function ImTestImagesInList(ImageList: TImageDataList): Boolean;
var
  I: LongInt;
  Arr: TDynImageDataArray;
begin
  Arr := nil;
  try
    Arr := PInternalList(ImageList).List;
    Result := True;
    for I := 0 to Length(Arr) - 1 do
    begin
      Result := Result and Imaging.TestImage(Arr[I]);
      if not Result then Break;
    end;
  except
    Result := False;
  end;
end;

function ImFreeImageList(var ImageList: TImageDataList): Boolean;
var
  Int: PInternalList;
begin
  try
    if ImageList <> nil then
    begin
      Int := PInternalList(ImageList);
      FreeImagesInArray(Int.List);
      Dispose(Int);
      ImageList := nil;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function ImLoadImageFromFile(FileName: PAnsiChar; var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.LoadImageFromFile(FileName, Image);
  except
    Result := False;
  end;
end;

function ImLoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.LoadImageFromMemory(Data, Size, Image);
  except
    Result := False;
  end;
end;

function ImLoadMultiImageFromFile(FileName: PAnsiChar; var ImageList: TImageDataList):
  Boolean;
begin
  try
    ImInitImageList(0, ImageList);
    Result := Imaging.LoadMultiImageFromFile(FileName,
      PInternalList(ImageList).List);
  except
    Result := False;
  end;
end;

function ImLoadMultiImageFromMemory(Data: Pointer; Size: LongInt;
  var ImageList: TImageDataList): Boolean;
begin
  try
    ImInitImageList(0, ImageList);
    Result := Imaging.LoadMultiImageFromMemory(Data, Size, PInternalList(ImageList).List);
  except
    Result := False;
  end;
end;

function ImSaveImageToFile(FileName: PAnsiChar; const Image: TImageData): Boolean;
begin
  try
    Result := Imaging.SaveImageToFile(FileName, Image);
  except
    Result := False;
  end;
end;

function ImSaveImageToMemory(Ext: PAnsiChar; Data: Pointer; var Size: LongInt;
  const Image: TImageData): Boolean;
begin
  try
    Result := Imaging.SaveImageToMemory(Ext, Data, Size, Image);
  except
    Result := False;
  end;
end;

function ImSaveMultiImageToFile(FileName: PAnsiChar;
  ImageList: TImageDataList): Boolean;
begin
  try
    Result := Imaging.SaveMultiImageToFile(FileName,
      PInternalList(ImageList).List);
  except
    Result := False;
  end;
end;

function ImSaveMultiImageToMemory(Ext: PAnsiChar; Data: Pointer; Size: PLongInt;
  ImageList: TImageDataList): Boolean;
begin
  try
    Result := Imaging.SaveMultiImageToMemory(Ext, Data, Size^,
      PInternalList(ImageList).List);
  except
    Result := False;
  end;
end;

function ImCloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
begin
  try
    Result := Imaging.CloneImage(Image, Clone);
  except
    Result := False;
  end;
end;

function ImConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean;
begin
  try
    Result := Imaging.ConvertImage(Image, DestFormat);
  except
    Result := False;
  end;
end;

function ImFlipImage(var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.FlipImage(Image);
  except
    Result := False;
  end;
end;

function ImMirrorImage(var Image: TImageData): Boolean;
begin
  try
    Result := Imaging.MirrorImage(Image);
  except
    Result := False;
  end;
end;

function ImResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt;
  Filter: TResizeFilter): Boolean;
begin
  try
    Result := Imaging.ResizeImage(Image, NewWidth, NewHeight, Filter);
  except
    Result := False;
  end;
end;

function ImSwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt):
  Boolean;
begin
  try
    Result := Imaging.SwapChannels(Image, SrcChannel, DstChannel);
  except
    Result := False;
  end;
end;

function ImReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean;
begin
  try
    Result := Imaging.ReduceColors(Image, MaxColors);
  except
    Result := False;
  end;
end;

function ImGenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TImageDataList): Boolean;
begin
  try
    ImInitImageList(0, MipMaps);
    Result := Imaging.GenerateMipMaps(Image, Levels,
      PInternalList(MipMaps).List);
  except
    Result := False;
  end;
end;

function ImMapImageToPalette(var Image: TImageData; Pal: PPalette32;
  Entries: LongInt): Boolean;
begin
  try
    Result := Imaging.MapImageToPalette(Image, Pal, Entries);
  except
    Result := False;
  end;
end;

function ImSplitImage(var Image: TImageData; var Chunks: TImageDataList;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: Pointer): Boolean;
begin
  try
    ImInitImageList(0, Chunks);
    Result := Imaging.SplitImage(Image, PInternalList(Chunks).List,
      ChunkWidth, ChunkHeight, XChunks, YChunks, PreserveSize, Fill);
  except
    Result := False;
  end;
end;

function ImMakePaletteForImages(Images: TImageDataList; Pal: PPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean;
begin
  try
    Result := Imaging.MakePaletteForImages(PInternalList(Images).List,
      Pal, MaxColors, ConvertImages);
  except
    Result := False;
  end;
end;

function ImRotateImage(var Image: TImageData; Angle: Single): Boolean;
begin
  Result := True;
  try
    Imaging.RotateImage(Image, Angle);
  except
    Result := False;
  end;
end;

function ImCopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean; cdecl;
begin
  try
    Result := Imaging.CopyRect(SrcImage, SrcX, SrcY, Width, Height,
      DstImage, DstX, DstY);
  except
    Result := False;
  end;
end;

function ImFillRect(var Image: TImageData; X, Y, Width, Height: LongInt;
  Fill: Pointer): Boolean;
begin
  try
    Result := Imaging.FillRect(Image, X, Y, Width, Height, Fill);
  except
    Result := False;
  end;
end;

function ImReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldPixel, NewPixel: Pointer): Boolean;
begin
  try
    Result := Imaging.ReplaceColor(Image, X, Y, Width, Height, OldPixel, NewPixel);
  except
    Result := False;
  end;
end;

function ImStretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean; cdecl;
begin
  try
    Result := Imaging.StretchRect(SrcImage, SrcX, SrcY, SrcWidth, SrcHeight,
      DstImage, DstX, DstY, DstWidth, DstHeight, Filter);
  except
    Result := False;
  end;
end;

procedure ImGetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
begin
  try
    Imaging.GetPixelDirect(Image, X, Y, Pixel);
  except
  end;
end;

procedure ImSetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
begin
  try
    Imaging.SetPixelDirect(Image, X, Y, Pixel);
  except
  end;
end;

function ImGetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec; cdecl;
begin
  try
    Result := Imaging.GetPixel32(Image, X, Y);
  except
    Result.Color := 0;
  end;
end;

procedure ImSetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec);
begin
  try
    Imaging.SetPixel32(Image, X, Y, Color);
  except
  end;
end;

function ImGetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec; cdecl;
begin
  try
    Result := Imaging.GetPixelFP(Image, X, Y);
  except
    FillChar(Result, SizeOf(Result), 0);
  end;
end;

procedure ImSetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec);
begin
  try
    Imaging.SetPixelFP(Image, X, Y, Color);
  except
  end;
end;

function ImNewPalette(Entries: LongInt; var Pal: PPalette32): Boolean;
begin
  try
    Imaging.NewPalette(Entries, Pal);
    Result := True;
  except
    Result := False;
  end;
end;

function ImFreePalette(var Pal: PPalette32): Boolean;
begin
  try
    Imaging.FreePalette(Pal);
    Result := True;
  except
    Result := False;
  end;
end;

function ImCopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt): Boolean;
begin
  try
    Imaging.CopyPalette(SrcPal, DstPal, SrcIdx, DstIdx, Count);
    Result := True;
  except
    Result := False;
  end;
end;

function ImFindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32): LongInt;
begin
  try
    Result := Imaging.FindColor(Pal, Entries, Color);
  except
    Result := 0;
  end;
end;

function ImFillGrayscalePalette(Pal: PPalette32; Entries: LongInt): Boolean;
begin
  try
    Imaging.FillGrayscalePalette(Pal, Entries);
    Result := True;
  except
    Result := False;
  end;
end;

function ImFillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte): Boolean;
begin
  try
    Imaging.FillCustomPalette(Pal, Entries, RBits, GBits, BBits, Alpha);
    Result := True;
  except
    Result := False;
  end;
end;

function ImSwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel,
  DstChannel: LongInt): Boolean;
begin
  try
    Imaging.SwapChannelsOfPalette(Pal, Entries, SrcChannel, DstChannel);
    Result := True;
  except
    Result := False;
  end;
end;

function ImSetOption(OptionId, Value: LongInt): Boolean;
begin
  try
    Result := Imaging.SetOption(OptionId, Value);
  except
    Result := False;
  end;
end;

function ImGetOption(OptionId: LongInt): LongInt;
begin
  try
    Result := GetOption(OptionId);
  except
    Result := InvalidOption;
  end;
end;

function ImPushOptions: Boolean;
begin
  try
    Result := Imaging.PushOptions;
  except
    Result := False;
  end;
end;

function ImPopOptions: Boolean;
begin
  try
    Result := Imaging.PopOptions;
  except
    Result := False;
  end;
end;

function ImGetImageFormatInfo(Format: TImageFormat; var Info: TImageFormatInfo): Boolean;
begin
  try
    Result := Imaging.GetImageFormatInfo(Format, Info);
  except
    Result := False;
  end;
end;

function ImGetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  try
    Result := Imaging.GetPixelsSize(Format, Width, Height);
  except
    Result := 0;
  end;
end;

procedure ImSetUserFileIO(OpenProc: TOpenProc; CloseProc: TCloseProc; EofProc: TEofProc; SeekProc: TSeekProc;
  TellProc: TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc);
begin
  try
    Imaging.SetUserFileIO(OpenProc, CloseProc, EofProc,
      SeekProc, TellProc, ReadProc, WriteProc);
  except
  end;
end;

procedure ImResetFileIO;
begin
  try
    Imaging.ResetFileIO;
  except
  end;
end;

{
  Changes/Bug Fixes:

  -- TODOS ----------------------------------------------------
    - nothing now 

  -- 0.77.1 ---------------------------------------------------
    - IO functions updates.

  -- 0.26.3 ---------------------------------------------------
    - changed PChars to PAnsiChars and some more D2009 friendly
      casts.

  -- 0.19 -----------------------------------------------------
    - updated to reflect changes in low level interface (added pixel set/get, ...)
    - changed ImInitImage to procedure to reflect change in Imaging.pas
    - added ImIsFileFormatSupported

  -- 0.15 -----------------------------------------------------
    - behaviour of ImGetImageListElement and ImSetImageListElement
      has changed - list items are now cloned rather than referenced,
      because of this ImFreeImageListKeepImages was no longer needed
      and was removed
    - many function headers were changed - mainly pointers were
      replaced with var and const parameters

  -- 0.13 -----------------------------------------------------
    - added TestImagesInList function and new 0.13 functions
    - images were not freed when image list was resized in ImSetImageListSize
    - ImSaveMultiImageTo* recreated the input image list with size = 0

}
end.

