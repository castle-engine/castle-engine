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

{ This unit is heart of Imaging library. It contains basic functions for
  manipulating image data as well as various image file format support.}
unit Imaging;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Types, ImagingTypes;

type
  { Default Imaging exception class }
  EImagingError = class(Exception);
  { Raised when function receives bad image (not passed TestImage).}
  EImagingBadImage = class(Exception)
  public
    constructor Create;
  end;

  { Dynamic array of TImageData records }
  TDynImageDataArray = array of TImageData;


{ ------------------------------------------------------------------------
                       Low Level Interface Functions
  ------------------------------------------------------------------------}

{ General Functions }

{ Initializes image (all is set to zeroes). Call this for each image
  before using it (before calling every other function) to be sure there
  are no random-filled bytes (which would cause errors later).}
procedure InitImage(out Image: TImageData);
{ Creates empty image of given dimensions and format. Image is filled with
  transparent black color (A=0, R=0, G=0, B=0).}
function NewImage(Width, Height: LongInt; Format: TImageFormat;
  var Image: TImageData): Boolean;
{ Returns True if given TImageData record is valid.}
function TestImage(const Image: TImageData): Boolean;
{ Frees given image data. After this call image is in the same state
  as after calling InitImage. If image is not valid (dost not pass TestImage
  test) it is only zeroed by calling InitImage.}
procedure FreeImage(var Image: TImageData);
{ Call FreeImage() on all images in given dynamic array and sets its
  length to zero.}
procedure FreeImagesInArray(var Images: TDynImageDataArray);
{ Returns True if all TImageData records in given array are valid. Returns False
  if at least one is invalid or if array is empty.}
function TestImagesInArray(const Images: TDynImageDataArray): Boolean;
{ Checks given file for every supported image file format and if
  the file is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If file is not in any of the supported formats empty string is returned.}
function DetermineFileFormat(const FileName: string): string;
{ Checks given stream for every supported image file format and if
  the stream is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If stream is not in any of the supported formats empty string is returned.}
function DetermineStreamFormat(Stream: TStream): string;
{ Checks given memory for every supported image file format and if
  the memory is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If memory is not in any of the supported formats empty string is returned.}
function DetermineMemoryFormat(Data: Pointer; Size: LongInt): string;
{ Checks that an appropriate file format is supported purely from inspecting
  the given file name's extension (not contents of the file itself).
  The file need not exist.}
function IsFileFormatSupported(const FileName: string): Boolean;
{ Enumerates all registered image file formats. Descriptive name,
  default extension, masks (like '*.jpg,*.jfif') and some capabilities
  of each format are returned. To enumerate all formats start with Index at 0 and
  call EnumFileFormats with given Index in loop until it returns False (Index is
  automatically increased by 1 in function's body on successful call).}
function EnumFileFormats(var Index: LongInt; var Name, DefaultExt, Masks: string;
  var CanSaveImages, IsMultiImageFormat: Boolean): Boolean;

{ Loading Functions }

{ Loads single image from given file.}
function LoadImageFromFile(const FileName: string; var Image: TImageData): Boolean;
{ Loads single image from given stream. If function fails stream position
  is not changed.}
function LoadImageFromStream(Stream: TStream; var Image: TImageData): Boolean;
{ Loads single image from given memory location.}
function LoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean;
{ Loads multiple images from given file.}
function LoadMultiImageFromFile(const FileName: string;
  var Images: TDynImageDataArray): Boolean;
{ Loads multiple images from given stream. If function fails stream position
  is not changed.}
function LoadMultiImageFromStream(Stream: TStream;
  var Images: TDynImageDataArray): Boolean;
{ Loads multiple images from given memory location.}
function LoadMultiImageFromMemory(Data: Pointer; Size: LongInt;
  var Images: TDynImageDataArray): Boolean;

{ Saving Functions }

{ Saves single image to given file.}
function SaveImageToFile(const FileName: string; const Image: TImageData): Boolean;
{ Saves single image to given stream. If function fails stream position
  is not changed. Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveImageToStream(const Ext: string; Stream: TStream;
  const Image: TImageData): Boolean;
{ Saves single image to given memory location. Memory must be allocated and its
  size is passed in Size parameter in which number of written bytes is returned.
  Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveImageToMemory(const Ext: string; Data: Pointer; var Size: LongInt;
  const Image: TImageData): Boolean;
{ Saves multiple images to given file. If format supports
  only single level images and there are multiple images to be saved,
  they are saved as sequence of files img000.jpg, img001.jpg ....).}
function SaveMultiImageToFile(const FileName: string;
  const Images: TDynImageDataArray): Boolean;
{ Saves multiple images to given stream. If format supports
  only single level images and there are multiple images to be saved,
  they are saved one after another to the stream. If function fails stream
  position is not changed. Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveMultiImageToStream(const Ext: string; Stream: TStream;
  const Images: TDynImageDataArray): Boolean;
{ Saves multiple images to given memory location. If format supports
  only single level images and there are multiple images to be saved,
  they are saved one after another to the memory. Memory must be allocated and
  its size is passed in Size parameter in which number of written bytes is returned.
  Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveMultiImageToMemory(const Ext: string; Data: Pointer;
  var Size: LongInt; const Images: TDynImageDataArray): Boolean;

{ Manipulation Functions }

{ Creates identical copy of image data. Clone should be initialized
  by InitImage or it should be valid image which will be freed by CloneImage.}
function CloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
{ Converts image to the given format.}
function ConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean;
{ Flips given image. Reverses the image along its horizontal axis - the top
  becomes the bottom and vice versa.}
function FlipImage(var Image: TImageData): Boolean;
{ Mirrors given image. Reverses the image along its vertical axis � the left
  side becomes the right and vice versa.}
function MirrorImage(var Image: TImageData): Boolean;
{ Resizes given image to new dimensions. Nearest, bilinear, or bicubic filtering
  can be used. Input Image must already be created - use NewImage to create new images.}
function ResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt;
  Filter: TResizeFilter): Boolean;
{ Swaps SrcChannel and DstChannel color or alpha channels of image.
  Use ChannelRed, ChannelBlue, ChannelGreen, ChannelAlpha constants to
  identify channels.}
function SwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean;
{ Reduces the number of colors of the Image. Currently MaxColors must be in
  range <2, 4096>. Color reduction works also for alpha channel. Note that for
  large images and big number of colors it can be very slow.
  Output format of the image is the same as input format.}
function ReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean;
{ Generates mipmaps for image. Levels is the number of desired mipmaps levels
  with zero (or some invalid number) meaning all possible levels.}
function GenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TDynImageDataArray): Boolean;
{ Maps image to existing palette producing image in ifIndex8 format.
  Pal must be allocated to at least Entries * SizeOf(TColor32Rec) bytes.
  As resulting image is in 8bit indexed format Entries must be lower or
  equal to 256.}
function MapImageToPalette(var Image: TImageData; Pal: PPalette32;
  Entries: LongInt): Boolean;
{ Splits image into XChunks x YChunks subimages. Default size of each chunk is
  ChunkWidth x ChunkHeight. If PreserveSize si True chunks at the edges of
  the image are also ChunkWidth x ChunkHeight sized and empty space is filled
  with optional Fill pixels. After calling this function XChunks contains number of
  chunks along x axis and YChunks along y axis. To access chunk [X, Y] use this
  index: Chunks[Y * XChunks + X].}
function SplitImage(var Image: TImageData; var Chunks: TDynImageDataArray;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: Pointer = nil): Boolean;
{ Creates palette with MaxColors based on the colors of images in Images array.
  Use it when you want to convert several images to indexed format using
  single palette for all of them. If ConvertImages is True images in array
  are converted to indexed format using resulting palette. if it is False
  images are left intact and only resulting palette is returned in Pal.
  Pal must be allocated to have at least MaxColors entries.}
function MakePaletteForImages(var Images: TDynImageDataArray; Pal: PPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean;
{ Rotates image by Angle degrees counterclockwise. All angles are allowed. }
procedure RotateImage(var Image: TImageData; Angle: Single);
{ Rotates image by Angle that is multiple of 90 degrees counterclockwise. }
procedure RotateImageMul90(var Image: TImageData; AngleDeg: Integer);

{ Drawing/Pixel functions }

{ Copies rectangular part of SrcImage to DstImage. No blending is performed -
  alpha is simply copied to destination image. Operates also with
  negative X and Y coordinates.
  Note that copying is fastest for images in the same data format
  (and slowest for images in special formats).}
function CopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean;
{ Fills given rectangle of image with given pixel fill data. Fill should point
  to the pixel in the same format as the given image is in.}
function FillRect(var Image: TImageData; X, Y, Width, Height: LongInt; FillColor: Pointer): Boolean;
{ Replaces pixels with OldPixel in the given rectangle by NewPixel.
  OldPixel and NewPixel should point to the pixels in the same format
  as the given image is in.}
function ReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldColor, NewColor: Pointer): Boolean;
{ Stretches the contents of the source rectangle to the destination rectangle
  with optional resampling. No blending is performed - alpha is
  simply copied/resampled to destination image. Note that stretching is
  fastest for images in the same data format (and slowest for
  images in special formats).}
function StretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean;
{ Copies pixel of Image at [X, Y] to memory pointed at by Pixel. Doesn't
  work with special formats.}
procedure GetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
{ Copies pixel from memory pointed at by Pixel to Image at position [X, Y].
  Doesn't work with special formats.}
procedure SetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to 32 bit ARGB. Works for all image formats (except special)
  so it is not very fast.}
function GetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec;
{ Procedure for setting pixel colors. Input 32 bit ARGB color is translated to
  native format and then written to Image. Works for all image formats (except special)
  so it is not very fast.}
procedure SetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to FP ARGB. Works for all image formats (except special)
  so it is not very fast.}
function GetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec;
{ Procedure for setting pixel colors. Input FP ARGB color is translated to
  native format and then written to Image. Works for all image formats (except special)
  so it is not very fast.}
procedure SetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); 

{ Palette Functions }

{ Allocates new palette with Entries ARGB color entries.}
procedure NewPalette(Entries: LongInt; var Pal: PPalette32);
{ Frees given palette.}
procedure FreePalette(var Pal: PPalette32);
{ Copies Count palette entries from SrcPal starting at index SrcIdx to
  DstPal at index DstPal.}
procedure CopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt);
{ Returns index of color in palette or index of nearest color if exact match
  is not found. Pal must have at least Entries color entries.}
function FindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32): LongInt;
{ Creates grayscale palette where each color channel has the same value.
  Pal must have at least Entries color entries.}
procedure FillGrayscalePalette(Pal: PPalette32; Entries: LongInt);
{ Creates palette with given bitcount for each channel.
  2^(RBits + GBits + BBits) should be equal to Entries. Examples:
  (3, 3, 2) will create palette with all possible colors of R3G3B2 format
  and (8, 0, 0) will create palette with 256 shades of red.
  Pal must be allocated to at least Entries * SizeOf(TColor32Rec) bytes.}
procedure FillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte = $FF);
{ Swaps SrcChannel and DstChannel color or alpha channels of palette.
  Use ChannelRed, ChannelBlue, ChannelGreen, ChannelAlpha constants to
  identify channels. Pal must be allocated to at least
  Entries * SizeOf(TColor32Rec) bytes.}
procedure SwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel,
  DstChannel: LongInt);

{ Options Functions }

{ Sets value of integer option specified by OptionId parameter.
  Option Ids are constants starting ImagingXXX.}
function SetOption(OptionId, Value: LongInt): Boolean;
{ Returns value of integer option specified by OptionId parameter. If OptionId is
  invalid, InvalidOption is returned. Option Ids are constants
  starting ImagingXXX.}
function GetOption(OptionId: LongInt): LongInt;
{ Pushes current values of all options on the stack. Returns True
  if successful (max stack depth is 8 now). }
function PushOptions: Boolean;
{ Pops back values of all options from the top of the stack. Returns True
  if successful (max stack depth is 8 now). }
function PopOptions: Boolean;

{ Image Data Format Functions }

{ Returns short information about given image format.}
function GetImageFormatInfo(Format: TImageFormat; out Info: TImageFormatInfo): Boolean;
{ Returns size in bytes of Width x Height area of pixels. Works for all formats.}
function GetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;

{ IO Functions }

{ User can set his own file IO functions used when loading from/saving to
  files by this function.}
procedure SetUserFileIO(OpenProc: TOpenProc; CloseProc: TCloseProc; EofProc: TEofProc; SeekProc:
  TSeekProc; TellProc: TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc);
{ Sets file IO functions to Imaging default.}
procedure ResetFileIO;

{ Raw Image IO Functions }

procedure ReadRawImageFromFile(const FileName: string; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageFromStream(Stream: TStream; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageFromMemory(Data: Pointer; DataSize: Integer; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);

procedure WriteRawImageToFile(const FileName: string; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageToStream(Stream: TStream; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageToMemory(Data: Pointer; DataSize: Integer; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  const Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);

{ Convenience/helper Functions }

{ Resizes image proportionally to fit the given width and height. }
procedure ResizeImageToFit(const SrcImage: TImageData; FitWidth, FitHeight: Integer;
  Filter: TResizeFilter; var DestImage: TImageData);

{ Color functions }

{ Constructs TColor24Rec color.}
function Color24(R, G, B: Byte): TColor24Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor32Rec color.}
function Color32(A, R, G, B: Byte): TColor32Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor48Rec color.}
function Color48(R, G, B: Word): TColor48Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColor64Rec color.}
function Color64(A, R, G, B: Word): TColor64Rec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColorFPRec color.}
function ColorFP(A, R, G, B: Single): TColorFPRec; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Constructs TColorHFRec color.}
function ColorHF(A, R, G, B: THalfFloat): TColorHFRec; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Convenience function for getting alpha component of TColor32.}
function GetAlphaValue(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Convenience function for getting red component of TColor32.}
function GetRedValue(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Convenience function for getting green component of TColor32.}
function GetGreenValue(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Convenience function for getting blue component of TColor32.}
function GetBlueValue(Color32: TColor32): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}


{ ------------------------------------------------------------------------
                           Other Imaging Stuff
  ------------------------------------------------------------------------}

type
  { Set of TImageFormat enum.}
  TImageFormats = set of TImageFormat;

  { Record containing set of IO functions internally used by image loaders/savers.}
  TIOFunctions = record
    Open: TOpenProc;
    Close: TCloseProc;
    Eof: TEofProc;
    Seek: TSeekProc;
    Tell: TTellProc;
    Read: TReadProc;
    Write: TWriteProc;
  end;
  PIOFunctions = ^TIOFunctions;

type
  TFileFormatFeature = (
    ffLoad,
    ffSave,
    ffMultiImage,
    ffReadOnSave,
    ffProgress,
    ffReadScanlines);

  TFileFormatFeatures = set of TFileFormatFeature;

  TMetadata = class;

  { Base class for various image file format loaders/savers which
    descend from this class. If you want to add support for new image file
    format the best way is probably to look at TImageFileFormat descendants'
    implementations that are already part of Imaging.}
{$TYPEINFO ON}
  TImageFileFormat = class
  private
    FExtensions: TStringList;
    FMasks: TStringList;
    function GetCanLoad: Boolean;
    function GetCanSave: Boolean;
    function GetIsMultiImageFormat: Boolean;
    { Does various checks and actions before LoadData method is called.}
    function PrepareLoad(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstFrame: Boolean): Boolean;
    { Processes some actions according to result of LoadData.}
    function PostLoadCheck(var Images: TDynImageDataArray; LoadResult: Boolean): Boolean;
    { Helper function to be called in SaveData methods of descendants (ensures proper
      index and sets FFirstIdx and FLastIdx for multi-images).}
    function PrepareSave(Handle: TImagingHandle; const Images: TDynImageDataArray;
      var Index: LongInt): Boolean;
    { Returns file open mode used for saving images. Depends on defined Features.}
    function GetSaveOpenMode: TOpenMode;
  protected
    FName: string;
    FFeatures: TFileFormatFeatures;
    FSupportedFormats: TImageFormats;
    FFirstIdx, FLastIdx: LongInt;
    FMetadata: TMetadata;
    { Descendants must override this method and define file format name and
      capabilities.}
    procedure Define; virtual;
    { Defines filename masks for this image file format. AMasks should be
      in format '*.ext1,*.ext2,umajo.*'.}
    procedure AddMasks(const AMasks: string);
    function GetFormatInfo(Format: TImageFormat): TImageFormatInfo;
    { Returns set of TImageData formats that can be saved in this file format
      without need for conversion.}
    function GetSupportedFormats: TImageFormats; virtual;
    { Method which must be overridden in descendants if they' are be capable
      of loading images. Images are already freed and length is set to zero
      whenever this method gets called. Also Handle is assured to be valid
      and contains data that passed TestFormat method's check.}
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstFrame: Boolean): Boolean; virtual;
    { Method which must be overridden in descendants if they are be capable
      of saving images. Images are checked to have length >0 and
      that they contain valid images. For single-image file formats
      Index contain valid index to Images array (to image which should be saved).
      Multi-image formats should use FFirstIdx and FLastIdx fields to
      to get all images that are to be saved.}
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; virtual;
    { This method is called internally by MakeCompatible when input image
      is in format not supported by this file format. Image is clone of
      MakeCompatible's input and Info is its extended format info.}
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); virtual;
    { Returns True if given image is supported for saving by this file format.
      Most file formats don't need to override this method. It checks
      (in this base class) if Image's format is in SupportedFormats set.
      But you may override it if you want further checks
      (proper width and height for example).}
    function IsSupported(const Image: TImageData): Boolean; virtual;
  public
    constructor Create(AMetadata: TMetadata = nil); virtual;
    destructor Destroy; override;

    { Loads images from file source.}
    function LoadFromFile(const FileName: string; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Loads images from stream source.}
    function LoadFromStream(Stream: TStream; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Loads images from memory source.}
    function LoadFromMemory(Data: Pointer; Size: LongInt;
      var Images: TDynImageDataArray; OnlyFirstLevel: Boolean = False): Boolean;

    { Saves images to file. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images (for example SaveToFile saves sequence of
      files img000.jpg, img001.jpg ....).}
    function SaveToFile(const FileName: string; const Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Saves images to stream. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images.}
    function SaveToStream(Stream: TStream; const Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Saves images to memory. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images. Data must be already allocated and their size passed
      as Size parameter, number of written bytes is then returned in the same
      parameter.}
    function SaveToMemory(Data: Pointer; var Size: LongInt;
      const Images: TDynImageDataArray; OnlyFirstLevel: Boolean = False): Boolean;

    { Makes Image compatible with this file format (that means it is in one
      of data formats in Supported formats set). If input is already
      in supported format then Compatible just use value from input
      (Compatible := Image) so must not free it after you are done with it
      (image bits pointer points to input image's bits).
      If input is not in supported format then it is cloned to Compatible
      and converted to one of supported formats (what exact format depends on
      this file format). If image is cloned MustBeFreed is set to True
      to indicated that you must free Compatible after you are done with it.}
    function MakeCompatible(const Image: TImageData; var Compatible: TImageData;
      out MustBeFreed: Boolean): Boolean;
    { Returns True if data located in source identified by Handle
      represent valid image in current format.}
    function TestFormat(Handle: TImagingHandle): Boolean; virtual;
    { Returns True if the given FileName matches filter for this file format.
      For most formats it just checks filename extensions.
      It uses filename masks in from Masks property so it can recognize
      filenames like this 'umajoXXXumajo.j0j' if one of the masks is
      'umajo*umajo.j?j'.}
    function TestFileName(const FileName: string): Boolean;
    { Descendants use this method to check if their options (registered with
      constant Ids for SetOption/GetOption interface or accessible as properties
      of descendants) have valid values and make necessary changes.}
    procedure CheckOptionsValidity; virtual;

    { Description of this format.}
    property Name: string read FName;
    { Indicates whether images in this format can be loaded.}
    property CanLoad: Boolean read GetCanLoad;
    { Indicates whether images in this format can be saved.}
    property CanSave: Boolean read GetCanSave;
    { Indicates whether images in this format can contain multiple image levels.}
    property IsMultiImageFormat: Boolean read GetIsMultiImageFormat;
    { List of filename extensions for this format.}
    property Extensions: TStringList read FExtensions;
    { List of filename masks that are used to associate filenames
      with TImageFileFormat descendants. Typical mask looks like
      '*.bmp' or 'texture.*' (supports file formats which use filename instead
      of extension to identify image files).}
    property Masks: TStringList read FMasks;
    { Set of TImageFormats supported by saving functions of this format. Images
      can be saved only in one those formats.}
    property SupportedFormats: TImageFormats read GetSupportedFormats;
  end;
{$TYPEINFO OFF}

  { Class reference for TImageFileFormat class}
  TImageFileFormatClass = class of TImageFileFormat;

  { Physical resolution unit.}
  TResolutionUnit = (
    ruSizeInMicroMeters, // value is pixel size in micrometers
    ruDpi,               // value is pixels/dots per inch
    ruDpm,               // value is pixels/dots per meter
    ruDpcm               // value is pixels/dots per centimeter
  );

  { Class for storage of single metadata item.}
  TMetadataItem = class
  public
    Id: string;
    ImageIndex: Integer;
    Value: Variant;
  end;

  { Metadata manager class.}
  TMetadata = class
  private
    FLoadMetaItems: TStringList;
    FSaveMetaItems: TStringList;
    procedure AddMetaToList(List: TStringList; const Id: string; const Value: Variant; ImageIndex: Integer);
    procedure ClearMetaList(List: TStringList);
    function GetMetaById(const Id: string): Variant;
    function GetMetaByIdMulti(const Id: string; ImageIndex: Integer): Variant;
    function GetMetaCount: Integer;
    function GetMetaByIdx(Index: Integer): TMetadataItem;
    function GetSaveMetaById(const Id: string): Variant;
    function GetSaveMetaByIdMulti(const Id: string; ImageIndex: Integer): Variant;
    procedure TranslateUnits(ResolutionUnit: TResolutionUnit; var XRes, YRes: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMetaItem(const Id: string; const Value: Variant; ImageIndex: Integer = 0);
    procedure SetMetaItemForSaving(const Id: string; const Value: Variant; ImageIndex: Integer = 0);
    function HasMetaItem(const Id: string; ImageIndex: Integer = 0): Boolean;
    function HasMetaItemForSaving(const Id: string; ImageIndex: Integer = 0): Boolean;

    procedure ClearMetaItems;
    procedure ClearMetaItemsForSaving;
    function GetMetaItemName(const Id: string; ImageIndex: Integer): string;
    { Copies loaded meta items to items-for-save stack. Use this when you want to
      save metadata that have been just loaded (e.g. resaving image in
      different file format but keeping the metadata).}
    procedure CopyLoadedMetaItemsForSaving;

    function GetPhysicalPixelSize(ResUnit: TResolutionUnit; out XSize,
      YSize: Double; MetaForSave: Boolean = False; ImageIndex: Integer = 0): Boolean;
    procedure SetPhysicalPixelSize(ResUnit: TResolutionUnit; XSize, YSize: Double;
      MetaForSave: Boolean = False; ImageIndex: Integer = 0);

    property MetaItems[const Id: string]: Variant read GetMetaById;
    property MetaItemsMulti[const Id: string; ImageIndex: Integer]: Variant read GetMetaByIdMulti;
    { Number of loaded metadata items.}
    property MetaItemCount: Integer read GetMetaCount;
    property MetaItemsByIdx[Index: Integer]: TMetadataItem read GetMetaByIdx;
    property MetaItemsForSaving[const Id: string]: Variant read GetSaveMetaById;
    property MetaItemsForSavingMulti[const Id: string; ImageIndex: Integer]: Variant read GetSaveMetaByIdMulti;
  end;

const
  { Metadata item id constants }

  { Physical size of one pixel in micrometers. Type of value is Double.}
  SMetaPhysicalPixelSizeX = 'PhysicalPixelSizeX';
  SMetaPhysicalPixelSizeY = 'PhysicalPixelSizeY';
  { Delay for frame of animation (how long it should stay visible) in milliseconds.
    Type of value is Integer.}
  SMetaFrameDelay = 'FrameDelay';
  { Number of times animation should be looped (0 = infinite looping). Type is Int. }
  SMetaAnimationLoops = 'AnimationLoops';
  { Gamma correction value. Type is Float.}
  SMetaGamma = 'Gamma';
  { Exposure value for HDR etc. Type is Float.}
  SMetaExposure = 'Exposure';
  { EXIF image metadata raw blob.}
  SMetaExifBlob = 'ExifBlob';
  { XMP image metadata raw blob.}
  SMetaXmpBlob  = 'XmpBlob';
  { IPTC image metadata raw blob.}
  SMetaIptcBlob = 'IptcBlob';

var
  GlobalMetadata: TMetadata;

{ Returns symbolic name of given format.}
function GetFormatName(Format: TImageFormat): string;
{ Returns string with information about given Image.}
function ImageToStr(const Image: TImageData): string;
{ Returns Imaging version string in format 'Major.Minor'.}
function GetVersionStr: string;
{ If Condition is True then TruePart is returned, otherwise FalsePart is returned.}
function IffFormat(Condition: Boolean; const TruePart, FalsePart: TImageFormat): TImageFormat;

{ Registers new option so it can be used by SetOption and GetOption functions.
  Returns True if registration was successful - that is Id is valid and is
  not already taken by another option.}
function RegisterOption(OptionId: LongInt; Variable: PLongInt): Boolean;

{ Registers new image loader/saver so it can be used by LoadFrom/SaveTo
  functions.}
procedure RegisterImageFileFormat(AClass: TImageFileFormatClass);
{ Returns image format loader/saver according to a given extension
  (case insensitive) or nil if not found. Extension may or may not
  contain the initial dot.}
function FindImageFileFormatByExt(const Ext: string): TImageFileFormat;
{ Returns image format loader/saver according to a given filename
  (case insensitive) or nil if not found. }
function FindImageFileFormatByName(const FileName: string): TImageFileFormat;
{ Returns image format loader/saver based on its class
  or nil if not found or not registered.}
function FindImageFileFormatByClass(AClass: TImageFileFormatClass): TImageFileFormat;
{ Returns number of registered image file format loaders/saver.}
function GetFileFormatCount: LongInt;
{ Returns image file format loader/saver at given index. Index must be
  in range [0..GetFileFormatCount - 1] otherwise nil is returned.}
function GetFileFormatAtIndex(Index: LongInt): TImageFileFormat;
{ Returns filter string for usage with open and save picture dialogs
  which contains all registered image file formats.
  Set OpenFileFilter to True if you want filter for open dialog
  and to False if you want save dialog filter (formats that cannot save to files
  are not added then).
  For open dialog filter for all known graphic files
  (like All(*.jpg;*.png;....) is added too at the first index.}
function GetImageFileFormatsFilter(OpenFileFilter: Boolean): string;
{ Returns file extension (without dot) of image format selected
  by given filter index. Used filter string is defined by GetImageFileFormatsFilter
  function. This function can be used with save dialogs (with filters created
  by GetImageFileFormatsFilter) to get the extension of file format selected
  in dialog quickly. Index is in range 1..N (as FilterIndex property
  of TOpenDialog/TSaveDialog)}
function GetFilterIndexExtension(Index: LongInt; OpenFileFilter: Boolean): string;
{ Returns filter index of image file format of file specified by FileName. Used filter
  string is defined by GetImageFileFormatsFilter function.
  Returned index is in range 1..N (as FilterIndex property of TOpenDialog/TSaveDialog)}
function GetFileNameFilterIndex(const FileName: string; OpenFileFilter: Boolean): LongInt;

{ Returns current IO functions.}
function GetIO: TIOFunctions;
{ Raises EImagingError with given message.}
procedure RaiseImaging(const Msg: string; const Args: array of const); overload;
procedure RaiseImaging(const Msg: string); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

const
  SImagingLibTitle = 'Vampyre Imaging Library';

implementation

uses
{$IFNDEF DONT_LINK_FILE_FORMATS}
{$IFNDEF DONT_LINK_BITMAP}
  ImagingBitmap,
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  ImagingJpeg,
{$ENDIF}
{$IF not Defined(DONT_LINK_PNG) or not Defined(DONT_LINK_MNG) or not Defined(DONT_LINK_JNG)}
  ImagingNetworkGraphics,
{$IFEND}
{$IFNDEF DONT_LINK_GIF}
  ImagingGif,
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  ImagingDds,
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  ImagingTarga,
{$ENDIF}
{$IFNDEF DONT_LINK_PNM}
  ImagingPortableMaps,
{$ENDIF}
{$IFNDEF DONT_LINK_RADHDR}
  ImagingRadiance,
{$ENDIF}
{$IFNDEF DONT_LINK_EXTRAS}
  ImagingExtFileFormats,
{$ENDIF}
{$ENDIF}
  //ImagingDebug,
  ImagingFormats, ImagingUtility, ImagingIO, Variants;

resourcestring
  SExceptMsg = 'Exception Message';
  SAllFilter = 'All Images';
  SUnknownFormat = 'Unknown and unsupported format';

  SErrorFreeImage = 'Error while freeing image. %s';
  SErrorCloneImage = 'Error while cloning image. %s';
  SErrorFlipImage = 'Error while flipping image. %s';
  SErrorMirrorImage = 'Error while mirroring image. %s';
  SErrorResizeImage = 'Error while resizing image.  %s';
  SErrorSwapImage = 'Error while swapping channels of image. %s';
  SFileFormatCanNotLoad = 'Image Format "%s" does not support loading images.';
  SFileFormatCanNotSave = 'Image Format "%s" does not support saving images.';
  SErrorNewImage = 'Error while creating image data with params: Width=%d ' +
    'Height=%d Format=%s.';
  SErrorConvertImage = 'Error while converting image to format "%s". %s';
  SImageInfo = 'Image @%p info: Width = %dpx, Height = %dpx, ' +
    'Format = %s, Size = %.0n %s, Bits @%p, Palette @%p.';
  SImageInfoInvalid = 'Access violation encountered when getting info on ' +
    'image at address %p.';
  SFileNotValid = 'File "%s" is not valid image in "%s" format.';
  SStreamNotValid = 'Stream %p does not contain valid image in "%s" format.';
  SMemoryNotValid = 'Memory %p (%d Bytes) does not contain valid image ' +
    'in "%s" format.';
  SErrorLoadingFile = 'Error while loading images from file "%s" (file format: %s).';
  SErrorLoadingStream = 'Error while loading images from stream %p (file format: %s).';
  SErrorLoadingMemory = 'Error while loading images from memory %p (%d Bytes) (file format: %s).';
  SErrorSavingFile = 'Error while saving images to file "%s" (file format: %s).';
  SErrorSavingStream = 'Error while saving images to stream %p (file format: %s).';
  SErrorSavingMemory = 'Error while saving images to memory %p (%d Bytes) (file format: %s).';
  SErrorFindColor = 'Error while finding color in palette @%p with %d entries.';
  SErrorGrayscalePalette = 'Error while filling grayscale palette @%p with %d entries.';
  SErrorCustomPalette = 'Error while filling custom palette @%p with %d entries.';
  SErrorSwapPalette = 'Error while swapping channels of palette @%p with %d entries.';
  SErrorReduceColors = 'Error while reducing number of colors of image to %d. %s';
  SErrorGenerateMipMaps = 'Error while generating %d mipmap levels for image %s';
  SImagesNotValid = 'One or more images are not valid.';
  SErrorCopyRect = 'Error while copying rect from image %s to image %s.';
  SErrorMapImage = 'Error while mapping image %s to palette.';
  SErrorFillRect = 'Error while filling rectangle X:%d Y:%d W:%d H:%d in image %s';
  SErrorSplitImage = 'Error while splitting image %s to %dx%d sized chunks.';
  SErrorMakePaletteForImages = 'Error while making %d color palette for %d images.';
  SErrorNewPalette = 'Error while creating new palette with %d entries';
  SErrorFreePalette = 'Error while freeing palette @%p';
  SErrorCopyPalette = 'Error while copying %d entries from palette @%p to @%p';
  SErrorReplaceColor = 'Error while replacing colors in rectangle X:%d Y:%d W:%d H:%d of image %s';
  SErrorRotateImage = 'Error while rotating image %s by %.2n degrees';
  SErrorStretchRect = 'Error while stretching rect from image %s to image %s.';
  SErrorEmptyStream = 'Input stream has no data. Check Position property.';
  SErrorInvalidInputImage = 'Invalid input image.';

  SErrorBadImage = 'Bad image detected.';

const
  // Initial size of array with options information
  InitialOptions = 256;
  // Max depth of the option stack
  OptionStackDepth = 8;
  // Do not change the default format now, its too late
  DefaultImageFormat: TImageFormat = ifA8R8G8B8;
  // Format used to create metadata IDs for frames loaded from multi-images.
  SMetaIdForSubImage = '%s/%d';

type
  TOptionArray = array of PLongInt;
  TOptionValueArray = array of LongInt;

  TOptionStack = class(TObject)
  private
    FStack: array[0..OptionStackDepth - 1] of TOptionValueArray;
    FPosition: LongInt;
  public
    constructor Create;
    destructor Destroy; override;
    function Push: Boolean;
    function Pop: Boolean;
  end;

var
  // Currently set IO functions
  IO: TIOFunctions;
  // List with all registered TImageFileFormat classes
  ImageFileFormats: TList = nil;
  // Array with registered options (pointers to their values)
  Options: TOptionArray = nil;
  // Array containing additional information about every image format
  ImageFormatInfos: TImageFormatInfoArray;
  // Stack used by PushOptions/PopOptions functions
  OptionStack: TOptionStack = nil;
var
  // Variable for ImagingColorReduction option
  ColorReductionMask: LongInt = $FF;
  // Variable for ImagingLoadOverrideFormat option
  LoadOverrideFormat: TImageFormat = ifUnknown;
  // Variable for ImagingSaveOverrideFormat option
  SaveOverrideFormat: TImageFormat = ifUnknown;
  // Variable for ImagingSaveOverrideFormat option
  MipMapFilter: TSamplingFilter = sfLinear;
  // Variable for ImagingBinaryThreshold option
  BinaryThreshold: Integer = 128;

{ Exceptions }

constructor EImagingBadImage.Create;
begin
  inherited Create(SErrorBadImage);
end;

{ Internal unit functions }

{ Modifies option value to be in the allowed range. Works only
  for options registered in this unit.}
function CheckOptionValue(OptionId, Value: LongInt): LongInt; forward;
{ Sets IO functions to file IO.}
procedure SetFileIO; forward;
{ Sets IO functions to stream IO.}
procedure SetStreamIO; forward;
{ Sets IO functions to memory IO.}
procedure SetMemoryIO; forward;
{ Inits image format infos array.}
procedure InitImageFormats; forward;
{ Free image format infos array.}
procedure FreeImageFileFormats; forward;
{ Creates options array and stack.}
procedure InitOptions; forward;
{ Frees options array and stack.}
procedure FreeOptions; forward;

function UpdateExceptMessage(E: Exception; const MsgToPrepend: string; const Args: array of const): Exception;
begin
  Result := E;
  E.Message := Format(MsgToPrepend, Args) +  ' ' + SExceptMsg + ': ' + E.Message
end;

{ ------------------------------------------------------------------------
                       Low Level Interface Functions
  ------------------------------------------------------------------------}

{ General Functions }

procedure InitImage(out Image: TImageData);
begin
  FillChar(Image, SizeOf(Image), 0);
end;

function NewImage(Width, Height: LongInt; Format: TImageFormat; var Image:
  TImageData): Boolean;
var
  FInfo: PImageFormatInfo;
begin
  Assert((Width > 0) and (Height >0));
  Assert(IsImageFormatValid(Format));
  Result := False;
  FreeImage(Image);
  try
    Image.Width := Width;
    Image.Height := Height;
    // Select default data format if selected
    if (Format = ifDefault)  then
      Image.Format := DefaultImageFormat
    else
      Image.Format := Format;
    // Get extended format info
    FInfo := ImageFormatInfos[Image.Format];
    if FInfo = nil then
    begin
      InitImage(Image);
      Exit;
    end;
    // Check image dimensions and calculate its size in bytes
    FInfo.CheckDimensions(FInfo.Format, Image.Width, Image.Height);
    Image.Size := FInfo.GetPixelsSize(FInfo.Format, Image.Width, Image.Height);
    if Image.Size = 0 then
    begin
      InitImage(Image);
      Exit;
    end;
    // Image bits are allocated and set to zeroes
    GetMem(Image.Bits, Image.Size);
    FillChar(Image.Bits^, Image.Size, 0);
    // Palette is allocated and set to zeroes
    if FInfo.PaletteEntries > 0 then
    begin
      GetMem(Image.Palette, FInfo.PaletteEntries * SizeOf(TColor32Rec));
      FillChar(Image.Palette^, FInfo.PaletteEntries * SizeOf(TColor32Rec), 0);
    end;
    Result := TestImage(Image);
  except
    on E: Exception do
    begin
      FreeMem(Image.Bits);
      FreeMem(Image.Palette);
      InitImage(Image);
      raise UpdateExceptMessage(E, SErrorNewImage, [Width, Height, GetFormatName(Format)]);
    end;
  end;
end;

function TestImage(const Image: TImageData): Boolean;
begin
  try
    Result := (LongInt(Image.Format) >= LongInt(Low(TImageFormat))) and
      (LongInt(Image.Format) <= LongInt(High(TImageFormat))) and
      (ImageFormatInfos[Image.Format] <> nil) and
      (Assigned(ImageFormatInfos[Image.Format].GetPixelsSize) and
      (ImageFormatInfos[Image.Format].GetPixelsSize(Image.Format,
      Image.Width, Image.Height) = Image.Size));
  except
    // Possible int overflows or other errors
    Result := False;
  end;
end;

procedure FreeImage(var Image: TImageData);
begin
  try
    if TestImage(Image) then
    begin
      FreeMemNil(Image.Bits);
      FreeMemNil(Image.Palette);
    end;
    InitImage(Image);
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorFreeImage, [ImageToStr(Image)]);
  end;
end;

procedure FreeImagesInArray(var Images: TDynImageDataArray);
var
  I: LongInt;
begin
  if Length(Images) > 0 then
  begin
    for I := 0 to Length(Images) - 1 do
      FreeImage(Images[I]);
    SetLength(Images, 0);
  end;
end;

function TestImagesInArray(const Images: TDynImageDataArray): Boolean;
var
  I: LongInt;
begin
  if Length(Images) > 0 then
  begin
    Result := True;
    for I := 0 to Length(Images) - 1 do
    begin
      Result := Result and TestImage(Images[I]);
      if not Result then
        Break;
    end;
  end
  else
    Result := False;
end;

function DetermineFileFormat(const FileName: string): string;
var
  I: LongInt;
  Fmt: TImageFileFormat;
  Handle: TImagingHandle;
begin
  Assert(FileName <> '');
  Result := '';
  SetFileIO;
  Handle := IO.Open(PChar(FileName), omReadOnly);
  try
    // First file format according to FileName and test if the data in
    // file is really in that format
    for I := 0 to ImageFileFormats.Count - 1 do
    begin
      Fmt := TImageFileFormat(ImageFileFormats[I]);
      if Fmt.TestFileName(FileName) and Fmt.TestFormat(Handle) then
      begin
        Result := Fmt.Extensions[0];
        Exit;
      end;
    end;
    // No file format was found with filename search so try data-based search
    for I := 0 to ImageFileFormats.Count - 1 do
    begin
      Fmt := TImageFileFormat(ImageFileFormats[I]);
      if Fmt.TestFormat(Handle) then
      begin
        Result := Fmt.Extensions[0];
        Exit;
      end;
    end;
  finally
    IO.Close(Handle);
  end;
end;

function DetermineStreamFormat(Stream: TStream): string;
var
  I: LongInt;
  Fmt: TImageFileFormat;
  Handle: TImagingHandle;
begin
  Assert(Stream <> nil);
  Result := '';
  SetStreamIO;
  Handle := IO.Open(Pointer(Stream), omReadOnly);
  try
    for I := 0 to ImageFileFormats.Count - 1 do
    begin
      Fmt := TImageFileFormat(ImageFileFormats[I]);
      if Fmt.TestFormat(Handle) then
      begin
        Result := Fmt.Extensions[0];
        Exit;
      end;
    end;
  finally
    IO.Close(Handle);
  end;
end;

function DetermineMemoryFormat(Data: Pointer; Size: LongInt): string;
var
  I: LongInt;
  Fmt: TImageFileFormat;
  Handle: TImagingHandle;
  IORec: TMemoryIORec;
begin
  Assert((Data <> nil) and (Size > 0));
  Result := '';
  SetMemoryIO;
  IORec.Data := Data;
  IORec.Position := 0;
  IORec.Size := Size;
  Handle := IO.Open(@IORec, omReadOnly);
  try
    for I := 0 to ImageFileFormats.Count - 1 do
    begin
      Fmt := TImageFileFormat(ImageFileFormats[I]);
      if Fmt.TestFormat(Handle) then
      begin
        Result := Fmt.Extensions[0];
        Exit;
      end;
    end;
  finally
    IO.Close(Handle);
  end;
end;

function IsFileFormatSupported(const FileName: string): Boolean;
begin
  Result := FindImageFileFormatByName(FileName) <> nil;
end;

function EnumFileFormats(var Index: LongInt; var Name, DefaultExt, Masks: string;
  var CanSaveImages, IsMultiImageFormat: Boolean): Boolean;
var
  FileFmt: TImageFileFormat;
begin
  FileFmt := GetFileFormatAtIndex(Index);
  Result := FileFmt <> nil;
  if Result then
  begin
    Name := FileFmt.Name;
    DefaultExt := FileFmt.Extensions[0];
    Masks := FileFmt.Masks.DelimitedText; 
    CanSaveImages := FileFmt.CanSave;
    IsMultiImageFormat := FileFmt.IsMultiImageFormat;
    Inc(Index);
  end
  else
  begin
    Name := '';
    DefaultExt := '';
    Masks := '';
    CanSaveImages := False;
    IsMultiImageFormat := False;
  end;
end;

{ Loading Functions }

function LoadImageFromFile(const FileName: string; var Image: TImageData):
  Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
  I: LongInt;
begin
  Assert(FileName <> '');
  Result := False;
  Format := FindImageFileFormatByExt(DetermineFileFormat(FileName));
  if Format <> nil then
  begin
    FreeImage(Image);
    Result := Format.LoadFromFile(FileName, IArray, True);
    if Result and (Length(IArray) > 0) then
    begin
      Image := IArray[0];
      for I := 1 to Length(IArray) - 1 do
        FreeImage(IArray[I]);
    end
    else
      Result := False;
  end;
end;

function LoadImageFromStream(Stream: TStream; var Image: TImageData): Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
  I: LongInt;
begin
  Assert(Stream <> nil);
  if Stream.Size - Stream.Position = 0 then
    RaiseImaging(SErrorEmptyStream, []);
  Result := False;
  Format := FindImageFileFormatByExt(DetermineStreamFormat(Stream));
  if Format <> nil then
  begin
    FreeImage(Image);
    Result := Format.LoadFromStream(Stream, IArray, True);
    if Result and (Length(IArray) > 0) then
    begin
      Image := IArray[0];
      for I := 1 to Length(IArray) - 1 do
        FreeImage(IArray[I]);
    end
    else
      Result := False;
  end;
end;

function LoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
  I: LongInt;
begin
  Assert((Data <> nil) and (Size > 0));
  Result := False;
  Format := FindImageFileFormatByExt(DetermineMemoryFormat(Data, Size));
  if Format <> nil then
  begin
    FreeImage(Image);
    Result := Format.LoadFromMemory(Data, Size, IArray, True);
    if Result and (Length(IArray) > 0) then
    begin
      Image := IArray[0];
      for I := 1 to Length(IArray) - 1 do
        FreeImage(IArray[I]);
    end
    else
      Result := False;
  end;
end;

function LoadMultiImageFromFile(const FileName: string; var Images:
  TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert(FileName <> '');
  Result := False;
  Format := FindImageFileFormatByExt(DetermineFileFormat(FileName));
  if Format <> nil then
  begin
    FreeImagesInArray(Images);
    Result := Format.LoadFromFile(FileName, Images);
  end;
end;

function LoadMultiImageFromStream(Stream: TStream; var Images: TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert(Stream <> nil);
  if Stream.Size - Stream.Position = 0 then
    RaiseImaging(SErrorEmptyStream, []);
  Result := False;
  Format := FindImageFileFormatByExt(DetermineStreamFormat(Stream));
  if Format <> nil then
  begin
    FreeImagesInArray(Images);
    Result := Format.LoadFromStream(Stream, Images);
  end;
end;

function LoadMultiImageFromMemory(Data: Pointer; Size: LongInt;
  var Images: TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert((Data <> nil) and (Size > 0));
  Result := False;
  Format := FindImageFileFormatByExt(DetermineMemoryFormat(Data, Size));
  if Format <> nil then
  begin
    FreeImagesInArray(Images);
    Result := Format.LoadFromMemory(Data, Size, Images);
  end;
end;

{ Saving Functions }

function SaveImageToFile(const FileName: string; const Image: TImageData): Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
begin
  Assert(FileName <> '');
  Result := False;
  Format := FindImageFileFormatByName(FileName);
  if Format <> nil then
  begin
    SetLength(IArray, 1);
    IArray[0] := Image;
    Result := Format.SaveToFile(FileName, IArray, True);
  end;
end;

function SaveImageToStream(const Ext: string; Stream: TStream;
  const Image: TImageData): Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
begin
  Assert((Ext <> '') and (Stream <> nil));
  Result := False;
  Format := FindImageFileFormatByExt(Ext);
  if Format <> nil then
  begin
    SetLength(IArray, 1);
    IArray[0] := Image;
    Result := Format.SaveToStream(Stream, IArray, True);
  end;
end;

function SaveImageToMemory(const Ext: string; Data: Pointer; var Size: LongInt;
  const Image: TImageData): Boolean;
var
  Format: TImageFileFormat;
  IArray: TDynImageDataArray;
begin
  Assert((Ext <> '') and (Data <> nil) and (Size > 0));
  Result := False;
  Format := FindImageFileFormatByExt(Ext);
  if Format <> nil then
  begin
    SetLength(IArray, 1);
    IArray[0] := Image;
    Result := Format.SaveToMemory(Data, Size, IArray, True);
  end;
end;

function SaveMultiImageToFile(const FileName: string;
  const Images: TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert(FileName <> '');
  Result := False;
  Format := FindImageFileFormatByName(FileName);
  if Format <> nil then
    Result := Format.SaveToFile(FileName, Images);
end;

function SaveMultiImageToStream(const Ext: string; Stream: TStream;
  const Images: TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert((Ext <> '') and (Stream <> nil));
  Result := False;
  Format := FindImageFileFormatByExt(Ext);
  if Format <> nil then
    Result := Format.SaveToStream(Stream, Images);
end;

function SaveMultiImageToMemory(const Ext: string; Data: Pointer;
  var Size: LongInt; const Images: TDynImageDataArray): Boolean;
var
  Format: TImageFileFormat;
begin
  Assert((Ext <> '') and (Data <> nil) and (Size > 0));
  Result := False;
  Format := FindImageFileFormatByExt(Ext);
  if Format <> nil then
    Result := Format.SaveToMemory(Data, Size, Images);
end;

{ Manipulation Functions }

function CloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
var
  Info: PImageFormatInfo;
begin
  Result := False;
  if TestImage(Image) then
  try
    if TestImage(Clone) and (Image.Bits <> Clone.Bits) then
      FreeImage(Clone)
    else
      InitImage(Clone);

    Info := ImageFormatInfos[Image.Format];
    Clone.Width := Image.Width;
    Clone.Height := Image.Height;
    Clone.Format := Image.Format;
    Clone.Size := Image.Size;

    if Info.PaletteEntries > 0 then
    begin
      GetMem(Clone.Palette, Info.PaletteEntries * SizeOf(TColor32Rec));
      Move(Image.Palette^, Clone.Palette^, Info.PaletteEntries *
        SizeOf(TColor32Rec));
    end;

    GetMem(Clone.Bits, Clone.Size);
    Move(Image.Bits^, Clone.Bits^, Clone.Size);
    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorCloneImage, [ImageToStr(Image)]);
  end;
end;

function ConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean;
var
  NewData: Pointer;
  NewPal: PPalette32;
  NewSize, NumPixels: LongInt;
  SrcInfo, DstInfo: PImageFormatInfo;
begin
  Assert(IsImageFormatValid(DestFormat));
  Result := False;
  if TestImage(Image) then
  with Image do
  try
    // If default format is set we use DefaultImageFormat
    if DestFormat = ifDefault then
      DestFormat := DefaultImageFormat;
    SrcInfo := ImageFormatInfos[Format];
    DstInfo := ImageFormatInfos[DestFormat];
    if SrcInfo = DstInfo then
    begin
      // There is nothing to convert - src is already in dest format
      Result := True;
      Exit;
    end;
    // Exit Src or Dest format is invalid 
    if (SrcInfo = nil) or (DstInfo = nil) then Exit;
    // If dest format is just src with swapped channels we call
    // SwapChannels instead
    if (SrcInfo.RBSwapFormat = DestFormat) and
      (DstInfo.RBSwapFormat = SrcInfo.Format) then
    begin
      Result := SwapChannels(Image, ChannelRed, ChannelBlue);
      Image.Format := SrcInfo.RBSwapFormat;
      Exit;
    end;

    if (not SrcInfo.IsSpecial) and (not DstInfo.IsSpecial) then
    begin
      NumPixels := Width * Height;
      NewSize := NumPixels * DstInfo.BytesPerPixel;
      GetMem(NewData, NewSize);
      FillChar(NewData^, NewSize, 0);
      GetMem(NewPal, DstInfo.PaletteEntries * SizeOf(TColor32Rec));
      FillChar(NewPal^, DstInfo.PaletteEntries * SizeOf(TColor32Rec), 0);

      if SrcInfo.IsIndexed then
      begin
        // Source: indexed format
        if DstInfo.IsIndexed then
          IndexToIndex(NumPixels, Bits, NewData, SrcInfo, DstInfo, Palette, NewPal)
        else if DstInfo.HasGrayChannel then
          IndexToGray(NumPixels, Bits, NewData, SrcInfo, DstInfo, Palette)
        else if DstInfo.IsFloatingPoint then
          IndexToFloat(NumPixels, Bits, NewData, SrcInfo, DstInfo, Palette)
        else
          IndexToChannel(NumPixels, Bits, NewData, SrcInfo, DstInfo, Palette);
      end
      else if SrcInfo.HasGrayChannel then
      begin
        // Source: grayscale format
        if DstInfo.IsIndexed then
          GrayToIndex(NumPixels, Bits, NewData, SrcInfo, DstInfo, NewPal)
        else if DstInfo.HasGrayChannel then
          GrayToGray(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else if DstInfo.IsFloatingPoint then
          GrayToFloat(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else
          GrayToChannel(NumPixels, Bits, NewData, SrcInfo, DstInfo);
      end
      else if SrcInfo.IsFloatingPoint then
      begin
        // Source: floating point format
        if DstInfo.IsIndexed then
          FloatToIndex(NumPixels, Bits, NewData, SrcInfo, DstInfo, NewPal)
        else if DstInfo.HasGrayChannel then
          FloatToGray(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else if DstInfo.IsFloatingPoint then
          FloatToFloat(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else
          FloatToChannel(NumPixels, Bits, NewData, SrcInfo, DstInfo);
      end
      else
      begin
        // Source: standard multi channel image
        if DstInfo.IsIndexed then
          ChannelToIndex(NumPixels, Bits, NewData, SrcInfo, DstInfo, NewPal)
        else if DstInfo.HasGrayChannel then
          ChannelToGray(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else if DstInfo.IsFloatingPoint then
          ChannelToFloat(NumPixels, Bits, NewData, SrcInfo, DstInfo)
        else
          ChannelToChannel(NumPixels, Bits, NewData, SrcInfo, DstInfo);
      end;

      FreeMemNil(Bits);
      FreeMemNil(Palette);
      Format := DestFormat;
      Bits := NewData;
      Size := NewSize;
      Palette := NewPal;
    end
    else
      ConvertSpecial(Image, SrcInfo, DstInfo);

    Assert(SrcInfo.Format <> Image.Format);

    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorConvertImage, [GetFormatName(DestFormat), ImageToStr(Image)]);
  end;
end;

function FlipImage(var Image: TImageData): Boolean;
var
  P1, P2, Buff: Pointer;
  WidthBytes, I: LongInt;
  OldFmt: TImageFormat;
begin
  Result := False;
  OldFmt := Image.Format;
  if TestImage(Image) then
  with Image do
  try
    if ImageFormatInfos[OldFmt].IsSpecial then
      ConvertImage(Image, ifDefault);

    WidthBytes := Width * ImageFormatInfos[Format].BytesPerPixel;
    GetMem(Buff, WidthBytes);
    try
      // Swap all scanlines of image
      for I := 0 to Height div 2 - 1 do
      begin
        P1 := @PByteArray(Bits)[I * WidthBytes];
        P2 := @PByteArray(Bits)[(Height - I - 1) * WidthBytes];
        Move(P1^, Buff^, WidthBytes);
        Move(P2^, P1^, WidthBytes);
        Move(Buff^, P2^, WidthBytes);
      end;
    finally
      FreeMemNil(Buff);
    end;

    if OldFmt <> Format then
      ConvertImage(Image, OldFmt);

    Result := True;
  except
    RaiseImaging(SErrorFlipImage, [ImageToStr(Image)]);
  end;
end;

function MirrorImage(var Image: TImageData): Boolean;
var
  Scanline: PByte;
  Buff: TColorFPRec;
  Bpp, Y, X, WidthDiv2, WidthBytes, XLeft, XRight: LongInt;
  OldFmt: TImageFormat;
begin
  Result := False;
  OldFmt := Image.Format;
  if TestImage(Image) then
  with Image do
  try
    if ImageFormatInfos[OldFmt].IsSpecial then
      ConvertImage(Image, ifDefault);

    Bpp := ImageFormatInfos[Format].BytesPerPixel;
    WidthDiv2 := Width div 2;
    WidthBytes := Width * Bpp;
    // Mirror all pixels on each scanline of image
    for Y := 0 to Height - 1 do
    begin
      Scanline := @PByteArray(Bits)[Y * WidthBytes];
      XLeft := 0;
      XRight := (Width - 1) * Bpp;
      for X := 0 to WidthDiv2 - 1 do
      begin
        CopyPixel(@PByteArray(Scanline)[XLeft], @Buff, Bpp);
        CopyPixel(@PByteArray(Scanline)[XRight],
          @PByteArray(Scanline)[XLeft], Bpp);
        CopyPixel(@Buff, @PByteArray(Scanline)[XRight], Bpp);
        Inc(XLeft, Bpp);
        Dec(XRight, Bpp);
      end;
    end;

    if OldFmt <> Format then
      ConvertImage(Image, OldFmt);

    Result := True;
  except
    RaiseImaging(SErrorMirrorImage, [ImageToStr(Image)]);
  end;
end;

function ResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt;
  Filter: TResizeFilter): Boolean;
var
  WorkImage: TImageData;
begin
  Assert((NewWidth > 0) and (NewHeight > 0), 'New width or height is zero.');
  Result := False;
  if TestImage(Image) and ((Image.Width <> NewWidth) or (Image.Height <> NewHeight)) then
  try
    InitImage(WorkImage);
    // Create new image with desired dimensions
    NewImage(NewWidth, NewHeight, Image.Format, WorkImage);
    // Stretch pixels from old image to new one
    StretchRect(Image, 0, 0, Image.Width, Image.Height,
      WorkImage, 0, 0, WorkImage.Width, WorkImage.Height, Filter);
    // Free old image and assign new image to it
    FreeMemNil(Image.Bits);
    if Image.Palette <> nil then
    begin
      FreeMem(WorkImage.Palette);
      WorkImage.Palette := Image.Palette;
    end;
    Image := WorkImage;
    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorResizeImage, [ImageToStr(Image)]);
  end;
end;

function SwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean;
var
  I, NumPixels: LongInt;
  Info: PImageFormatInfo;
  Swap, Alpha: Word;
  Data: PByte;
  Pix64: TColor64Rec;
  PixF: TColorFPRec;
  SwapF: Single;
begin
  Assert((SrcChannel in [0..3]) and (DstChannel in [0..3]));
  Result := False;
  if TestImage(Image) and (SrcChannel <> DstChannel) then
  with Image do
  try
    NumPixels := Width * Height;
    Info := ImageFormatInfos[Format];
    Data := Bits;

    if (Info.Format = ifR8G8B8) or ((Info.Format = ifA8R8G8B8) and
       (SrcChannel <> ChannelAlpha) and (DstChannel <> ChannelAlpha)) then
    begin
      // Swap channels of most common formats R8G8B8 and A8R8G8B8 (no alpha)
      for I := 0 to NumPixels - 1 do
      with PColor24Rec(Data)^ do
      begin
        Swap := Channels[SrcChannel];
        Channels[SrcChannel] := Channels[DstChannel];
        Channels[DstChannel] := Swap;
        Inc(Data, Info.BytesPerPixel);
      end;
    end
    else if Info.IsIndexed then
    begin
      // Swap palette channels of indexed images
      SwapChannelsOfPalette(Palette, Info.PaletteEntries, SrcChannel, DstChannel)
    end
    else if Info.IsFloatingPoint then
    begin
      // Swap channels of floating point images
      for I := 0 to NumPixels - 1 do
      begin
        FloatGetSrcPixel(Data, Info, PixF);
        with PixF do
        begin
          SwapF := Channels[SrcChannel];
          Channels[SrcChannel] := Channels[DstChannel];
          Channels[DstChannel] := SwapF;
        end;
        FloatSetDstPixel(Data, Info, PixF);
        Inc(Data, Info.BytesPerPixel);
      end;
    end
    else if Info.IsSpecial then
    begin
      // Swap channels of special format images
      ConvertImage(Image, ifDefault);
      SwapChannels(Image, SrcChannel, DstChannel);
      ConvertImage(Image, Info.Format);
    end
    else if Info.HasGrayChannel and Info.HasAlphaChannel and
      ((SrcChannel = ChannelAlpha) or (DstChannel = ChannelAlpha)) then
    begin
      for I := 0 to NumPixels - 1 do
      begin
        // If we have grayscale image with alpha and alpha is channel
        // to be swapped, we swap it. No other alternative for gray images,
        // just alpha and something
        GrayGetSrcPixel(Data, Info, Pix64, Alpha);
        Swap := Alpha;
        Alpha := Pix64.A;
        Pix64.A := Swap;
        GraySetDstPixel(Data, Info, Pix64, Alpha);
        Inc(Data, Info.BytesPerPixel);
      end;
    end
    else
    begin
      // Then do general swap on other channel image formats
      for I := 0 to NumPixels - 1 do
      begin
        ChannelGetSrcPixel(Data, Info, Pix64);
        with Pix64 do
        begin
          Swap := Channels[SrcChannel];
          Channels[SrcChannel] := Channels[DstChannel];
          Channels[DstChannel] := Swap;
        end;
        ChannelSetDstPixel(Data, Info, Pix64);
        Inc(Data, Info.BytesPerPixel);
      end;
    end;

    Result := True;
  except
    RaiseImaging(SErrorSwapImage, [ImageToStr(Image)]);
  end;
end;

function ReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean;
var
  TmpInfo: TImageFormatInfo;
  Data, Index: PWord;
  I, NumPixels: LongInt;
  Pal: PPalette32;
  Col:PColor32Rec;
  OldFmt: TImageFormat;
begin
  Result := False;
  if TestImage(Image) then
  with Image do
  try
    // First create temp image info and allocate output bits and palette
    MaxColors := ClampInt(MaxColors, 2, High(Word));
    OldFmt := Format;
    FillChar(TmpInfo, SizeOf(TmpInfo), 0);
    TmpInfo.PaletteEntries := MaxColors;
    TmpInfo.BytesPerPixel := 2;
    NumPixels := Width * Height;
    GetMem(Data, NumPixels * TmpInfo.BytesPerPixel);
    GetMem(Pal, MaxColors * SizeOf(TColor32Rec));
    ConvertImage(Image, ifA8R8G8B8);
    // We use median cut algorithm to create reduced palette and to
    // fill Data with indices to this palette
    ReduceColorsMedianCut(NumPixels, Bits, PByte(Data),
      ImageFormatInfos[Format], @TmpInfo, MaxColors, ColorReductionMask, Pal);
    Col := Bits;
    Index := Data;
    // Then we write reduced colors to the input image
    for I := 0 to NumPixels - 1 do
    begin
      Col.Color := Pal[Index^].Color;
      Inc(Col);
      Inc(Index);
    end;
    FreeMemNil(Data);
    FreeMemNil(Pal);
    // And convert it to its original format
    ConvertImage(Image, OldFmt);
    Result := True;
  except
    RaiseImaging(SErrorReduceColors, [MaxColors, ImageToStr(Image)]);
  end;
end;

function GenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TDynImageDataArray): Boolean;
var
  Width, Height, I, Count: LongInt;
  Info: TImageFormatInfo;
  CompatibleCopy: TImageData;
begin
  Result := False;
  if TestImage(Image) then
  try
    Width := Image.Width;
    Height := Image.Height;
    // We compute number of possible mipmap levels and if
    // the given levels are invalid or zero we use this value
    Count := GetNumMipMapLevels(Width, Height);
    if (Levels <= 0) or (Levels > Count) then
      Levels := Count;

    // If we have special format image we create copy to allow pixel access.
    // This is also done in FillMipMapLevel which is called for each level
    // but then the main big image would be converted to compatible
    // for every level.
    GetImageFormatInfo(Image.Format, Info);
    if Info.IsSpecial then
    begin
      InitImage(CompatibleCopy);
      CloneImage(Image, CompatibleCopy);
      ConvertImage(CompatibleCopy, ifDefault);
    end
    else
      CompatibleCopy := Image;

    FreeImagesInArray(MipMaps);
    SetLength(MipMaps, Levels);
    CloneImage(Image, MipMaps[0]);

    for I := 1 to Levels - 1 do
    begin
      Width := Width shr 1;
      Height := Height shr 1;
      if Width < 1 then Width := 1;
      if Height < 1 then Height := 1;
      FillMipMapLevel(CompatibleCopy, Width, Height, MipMaps[I]);
    end;

    if CompatibleCopy.Format <> MipMaps[0].Format then
    begin
      // Must convert smaller levels to proper format
      for I := 1 to High(MipMaps) do
        ConvertImage(MipMaps[I], MipMaps[0].Format);
      FreeImage(CompatibleCopy);
    end;

    Result := True;
  except
    RaiseImaging(SErrorGenerateMipMaps, [Levels, ImageToStr(Image)]);
  end;
end;

function MapImageToPalette(var Image: TImageData; Pal: PPalette32;
  Entries: LongInt): Boolean;

  function FindNearestColor(Pal: PPalette32; Entries: LongInt; Col: TColor32Rec): LongInt;
  var
    I, MinDif, Dif: LongInt;
  begin
    Result := 0;
    MinDif := 1020;
    for I := 0 to Entries - 1 do
    with Pal[I] do
    begin
      Dif := Abs(R - Col.R);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(G - Col.G);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(B - Col.B);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(A - Col.A);
      if Dif < MinDif then
      begin
        MinDif := Dif;
        Result := I;
      end;
    end;
  end;

var
  I, MaxEntries: LongInt;
  PIndex: PByte;
  PColor: PColor32Rec;
  CloneARGB: TImageData;
  Info: PImageFormatInfo;
begin
  Assert((Entries >= 2) and (Entries <= 256));
  Result := False;

  if TestImage(Image) then
  try
    // We create clone of source image in A8R8G8B8 and
    // then recreate source image in ifIndex8 format
    // with palette taken from Pal parameter
    InitImage(CloneARGB);
    CloneImage(Image, CloneARGB);
    ConvertImage(CloneARGB, ifA8R8G8B8);
    FreeImage(Image);
    NewImage(CloneARGB.Width, CloneARGB.Height, ifIndex8, Image);

    Info := ImageFormatInfos[Image.Format];
    MaxEntries := Min(Info.PaletteEntries, Entries);
    Move(Pal^, Image.Palette^, MaxEntries * SizeOf(TColor32Rec));
    PIndex := Image.Bits;
    PColor := CloneARGB.Bits;

    // For every pixel of ARGB clone we find closest color in
    // given palette and assign its index to resulting image's pixel
    // procedure used here is very slow but simple and memory usage friendly
    // (contrary to other methods)
    for I := 0 to Image.Width * Image.Height - 1 do
    begin
      PIndex^ := Byte(FindNearestColor(Image.Palette, MaxEntries, PColor^));
      Inc(PIndex);
      Inc(PColor);
    end;

    FreeImage(CloneARGB);
    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorMapImage, [ImageToStr(Image)]);
  end;
end;

function SplitImage(var Image: TImageData; var Chunks: TDynImageDataArray;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: Pointer): Boolean;
var
  X, Y, XTrunc, YTrunc: LongInt;
  NotOnEdge: Boolean;
  Info: PImageFormatInfo;
  OldFmt: TImageFormat;

begin
  Assert((ChunkWidth > 0) and (ChunkHeight > 0));
  Result := False;
  OldFmt := Image.Format;
  FreeImagesInArray(Chunks);

  if TestImage(Image) then
  try
    Info := ImageFormatInfos[Image.Format];
    if Info.IsSpecial then
      ConvertImage(Image, ifDefault);

    // We compute make sure that chunks are not larger than source image or negative
    ChunkWidth := ClampInt(ChunkWidth, 0, Image.Width);
    ChunkHeight := ClampInt(ChunkHeight, 0, Image.Height);
    // Number of chunks along X and Y axes is computed
    XChunks := Ceil(Image.Width / ChunkWidth);
    YChunks := Ceil(Image.Height / ChunkHeight);
    SetLength(Chunks, XChunks * YChunks);

    // For every chunk we create new image and copy a portion of
    // the source image to it. If chunk is on the edge of the source image
    // we fill empty space with Fill pixel data if PreserveSize is set or
    // make the chunk smaller if it is not set
    for Y := 0 to YChunks - 1 do
      for X := 0 to XChunks - 1 do
      begin
        // Determine if current chunk is on the edge of original image
        NotOnEdge := ((X < XChunks - 1) and (Y < YChunks - 1)) or
          ((Image.Width mod ChunkWidth = 0) and (Image.Height mod ChunkHeight = 0));

        if PreserveSize or NotOnEdge then
        begin
          // We should preserve chunk sizes or we are somewhere inside original image
          NewImage(ChunkWidth, ChunkHeight, Image.Format, Chunks[Y * XChunks + X]);
          if (not NotOnEdge) and (Fill <> nil) then
            FillRect(Chunks[Y * XChunks + X], 0, 0, ChunkWidth, ChunkHeight, Fill);
          CopyRect(Image, X * ChunkWidth, Y * ChunkHeight, ChunkWidth, ChunkHeight,
            Chunks[Y * XChunks + X], 0, 0);
        end
        else
        begin
          // Create smaller edge chunk
          XTrunc := Image.Width - X * ChunkWidth;
          YTrunc := Image.Height - Y * ChunkHeight;
          NewImage(XTrunc, YTrunc, Image.Format, Chunks[Y * XChunks + X]);
          CopyRect(Image, X * ChunkWidth, Y * ChunkHeight, XTrunc, YTrunc,
            Chunks[Y * XChunks + X], 0, 0);
        end;
        
        // If source image is in indexed format we copy its palette to chunk
        if Info.IsIndexed then
        begin
          Move(Image.Palette^, Chunks[Y * XChunks + X].Palette^,
            Info.PaletteEntries * SizeOf(TColor32Rec));
        end;
      end;

    if OldFmt <> Image.Format then
    begin
      ConvertImage(Image, OldFmt);
      for X := 0 to Length(Chunks) - 1 do
        ConvertImage(Chunks[X], OldFmt);
    end;

    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorSplitImage,
      [ImageToStr(Image), ChunkWidth, ChunkHeight]);
  end;
end;

function MakePaletteForImages(var Images: TDynImageDataArray; Pal: PPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean;
var
  I: Integer;
  SrcInfo, DstInfo: PImageFormatInfo;
  Target, TempImage: TImageData;
  DstFormat: TImageFormat;
begin
  Assert((Pal <> nil) and (MaxColors > 0));
  Result := False;
  InitImage(TempImage);

  if TestImagesInArray(Images) then
  try
    // Null the color histogram
    ReduceColorsMedianCut(0, nil, nil, nil, nil, 0, 0, nil, [raCreateHistogram]);
    for I := 0 to Length(Images) - 1 do
    begin
      SrcInfo := ImageFormatInfos[Images[I].Format];
      if SrcInfo.IsIndexed or SrcInfo.IsSpecial then
      begin
        // create temp image in supported format for updating histogram
        CloneImage(Images[I], TempImage);
        ConvertImage(TempImage, ifA8R8G8B8);
        SrcInfo := ImageFormatInfos[TempImage.Format];
      end
      else
        TempImage := Images[I];

      // Update histogram with colors of each input image
      ReduceColorsMedianCut(TempImage.Width * TempImage.Height, TempImage.Bits,
        nil, SrcInfo, nil, MaxColors, ColorReductionMask, nil, [raUpdateHistogram]);

      if Images[I].Bits <> TempImage.Bits then
        FreeImage(TempImage);
    end;
    // Construct reduced color map from the histogram
    ReduceColorsMedianCut(0, nil, nil, nil, nil, MaxColors, ColorReductionMask,
      Pal, [raMakeColorMap]);

    if ConvertImages then
    begin
      DstFormat := ifIndex8;
      DstInfo := ImageFormatInfos[DstFormat];
      MaxColors := Min(DstInfo.PaletteEntries, MaxColors);

      for I := 0 to Length(Images) - 1 do
      begin
        SrcInfo := ImageFormatInfos[Images[I].Format];
        if SrcInfo.IsIndexed or SrcInfo.IsSpecial then
        begin
          // If source image is in format not supported by ReduceColorsMedianCut
          // we convert it
          ConvertImage(Images[I], ifA8R8G8B8);
          SrcInfo := ImageFormatInfos[Images[I].Format];
        end;

        InitImage(Target);
        NewImage(Images[I].Width, Images[I].Height, DstFormat, Target);
        // We map each input image to reduced palette and replace
        // image in array with mapped image
        ReduceColorsMedianCut(Images[I].Width * Images[I].Height, Images[I].Bits,
          Target.Bits, SrcInfo, DstInfo, MaxColors, 0, nil, [raMapImage]);
        Move(Pal^, Target.Palette^, MaxColors * SizeOf(TColor32Rec));

        FreeImage(Images[I]);
        Images[I] := Target;
      end;
    end;
    Result := True;
  except
    RaiseImaging(SErrorMakePaletteForImages, [MaxColors, Length(Images)]);
  end;
end;

procedure RotateImage(var Image: TImageData; Angle: Single);
var
  OldFmt: TImageFormat;

  procedure XShear(var Src, Dst: TImageData; Row, Offset, Weight, Bpp: Integer);
  var
    I, J, XPos: Integer;
    PixSrc, PixLeft, PixOldLeft: TColor32Rec;
    LineDst: PByteArray;
    SrcPtr: PColor32;
  begin
    SrcPtr := @PByteArray(Src.Bits)[Row * Src.Width * Bpp];
    LineDst := @PByteArray(Dst.Bits)[Row * Dst.Width * Bpp];
    PixOldLeft.Color := 0;

    for I := 0 to Src.Width - 1 do
    begin
      CopyPixel(SrcPtr, @PixSrc, Bpp);
      for J := 0 to Bpp - 1 do
        PixLeft.Channels[J] := MulDiv(PixSrc.Channels[J], Weight, 256);

      XPos := I + Offset;
      if (XPos >= 0) and (XPos < Dst.Width) then
      begin
        for J := 0 to Bpp - 1 do
          PixSrc.Channels[J] := ClampToByte(PixSrc.Channels[J] - (PixLeft.Channels[J] - PixOldLeft.Channels[J]));
        CopyPixel(@PixSrc, @LineDst[XPos * Bpp], Bpp);
      end;
      PixOldLeft := PixLeft;
      Inc(PByte(SrcPtr), Bpp);
    end;

    XPos := Src.Width + Offset;
    if XPos < Dst.Width then
      CopyPixel(@PixOldLeft, @LineDst[XPos * Bpp], Bpp);
  end;

  procedure YShear(var Src, Dst: TImageData; Col, Offset, Weight, Bpp: Integer);
  var
    I, J, YPos: Integer;
    PixSrc, PixLeft, PixOldLeft: TColor32Rec;
    SrcPtr: PByte;
  begin
    SrcPtr := @PByteArray(Src.Bits)[Col * Bpp];
    PixOldLeft.Color := 0;

    for I := 0 to Src.Height - 1 do
    begin
      CopyPixel(SrcPtr, @PixSrc, Bpp);
      for J := 0 to Bpp - 1 do
        PixLeft.Channels[J] := MulDiv(PixSrc.Channels[J], Weight, 256);

      YPos := I + Offset;
      if (YPos >= 0) and (YPos < Dst.Height) then
      begin
        for J := 0 to Bpp - 1 do
          PixSrc.Channels[J] := ClampToByte(PixSrc.Channels[J] - (PixLeft.Channels[J] - PixOldLeft.Channels[J]));
        CopyPixel(@PixSrc, @PByteArray(Dst.Bits)[(YPos * Dst.Width + Col) * Bpp], Bpp);
      end;
      PixOldLeft := PixLeft;
      Inc(SrcPtr, Src.Width * Bpp);
    end;

    YPos := Src.Height + Offset;
    if YPos < Dst.Height then
      CopyPixel(@PixOldLeft, @PByteArray(Dst.Bits)[(YPos * Dst.Width + Col) * Bpp], Bpp);
  end;

  procedure Rotate45(var Image: TImageData; Angle: Single);
  var
    TempImage1, TempImage2: TImageData;
    AngleRad, AngleTan, AngleSin, AngleCos, Shear: Single;
    I, DstWidth, DstHeight, SrcWidth, SrcHeight, Bpp: Integer;
    SrcFmt, TempFormat: TImageFormat;
    Info: TImageFormatInfo;
  begin
    AngleRad := Angle * Pi / 180;
    AngleSin := Sin(AngleRad);
    AngleCos := Cos(AngleRad);
    AngleTan := Sin(AngleRad / 2) / Cos(AngleRad / 2);
    SrcWidth := Image.Width;
    SrcHeight := Image.Height;
    SrcFmt := Image.Format;

    if not (SrcFmt in [ifR8G8B8..ifX8R8G8B8, ifGray8..ifGray32, ifA16Gray16]) then
      ConvertImage(Image, ifA8R8G8B8);

    TempFormat := Image.Format;
    GetImageFormatInfo(TempFormat, Info);
    Bpp := Info.BytesPerPixel;

    // 1st shear (horizontal)
    DstWidth := Trunc(SrcWidth + SrcHeight * Abs(AngleTan) + 0.5);
    DstHeight := SrcHeight;
    InitImage(TempImage1);
    NewImage(DstWidth, DstHeight, TempFormat, TempImage1);

    for I := 0 to DstHeight - 1 do
    begin
      if AngleTan >= 0 then
        Shear := (I + 0.5) * AngleTan
      else
        Shear := (I - DstHeight + 0.5) * AngleTan;
      XShear(Image, TempImage1, I, Floor(Shear), Trunc(255 * (Shear - Floor(Shear)) + 1), Bpp);
    end;

    // 2nd shear (vertical)
    FreeImage(Image);
    DstHeight := Trunc(SrcWidth * Abs(AngleSin) + SrcHeight * AngleCos + 0.5) + 1;
    InitImage(TempImage2);
    NewImage(DstWidth, DstHeight, TempFormat, TempImage2);

    if AngleSin >= 0 then
      Shear := (SrcWidth - 1) * AngleSin
    else
      Shear := (SrcWidth - DstWidth) * -AngleSin;

    for I := 0 to DstWidth - 1 do
    begin
      YShear(TempImage1, TempImage2, I, Floor(Shear), Trunc(255 * (Shear - Floor(Shear)) + 1), Bpp);
      Shear := Shear - AngleSin;
    end;

    // 3rd shear (horizontal)
    FreeImage(TempImage1);
    DstWidth := Trunc(SrcHeight * Abs(AngleSin) + SrcWidth * AngleCos + 0.5) + 1;
    NewImage(DstWidth, DstHeight, TempFormat, Image);

    if AngleSin >= 0 then
      Shear := (SrcWidth - 1) * AngleSin * -AngleTan
    else
      Shear := ((SrcWidth - 1) * -AngleSin + (1 - DstHeight)) * AngleTan;

    for I := 0 to DstHeight - 1 do
    begin
      XShear(TempImage2, Image, I, Floor(Shear), Trunc(255 * (Shear - Floor(Shear)) + 1), Bpp);
      Shear := Shear + AngleTan;
    end;

    FreeImage(TempImage2);
    if Image.Format <> SrcFmt then
      ConvertImage(Image, SrcFmt);
  end;

  procedure RotateMul90(var Image: TImageData; Angle: Integer);
  var
    RotImage: TImageData;
    X, Y, BytesPerPixel: Integer;
    RotPix, Pix: PByte;
  begin
    InitImage(RotImage);
    BytesPerPixel := ImageFormatInfos[Image.Format].BytesPerPixel;

    if ((Angle = 90) or (Angle = 270)) and (Image.Width <> Image.Height) then
      NewImage(Image.Height, Image.Width, Image.Format, RotImage)
    else
      NewImage(Image.Width, Image.Height, Image.Format, RotImage);

    RotPix := RotImage.Bits;
    case Angle of
      90:
        begin
          for Y := 0 to RotImage.Height - 1 do
          begin
            Pix := @PByteArray(Image.Bits)[(Image.Width - Y - 1) * BytesPerPixel];
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Inc(Pix, Image.Width * BytesPerPixel);
            end;
          end;
        end;
      180:
        begin
          Pix := @PByteArray(Image.Bits)[((Image.Height - 1) * Image.Width +
            (Image.Width - 1)) * BytesPerPixel];
          for Y := 0 to RotImage.Height - 1 do
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Dec(Pix, BytesPerPixel);
            end;
        end;
      270:
        begin
          for Y := 0 to RotImage.Height - 1 do
          begin
            Pix := @PByteArray(Image.Bits)[((Image.Height - 1) * Image.Width + Y) * BytesPerPixel];
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Dec(Pix, Image.Width * BytesPerPixel);
            end;
          end;
        end;
    end;

    FreeMemNil(Image.Bits);
    RotImage.Palette := Image.Palette;
    Image := RotImage;
  end;

begin
  if TestImage(Image) then
  try
    while Angle >= 360 do
      Angle := Angle - 360;
    while Angle < 0 do
      Angle := Angle + 360;

    if (Angle = 0) or (Abs(Angle) = 360) then
      Exit;

    OldFmt := Image.Format;
    if ImageFormatInfos[Image.Format].IsSpecial then
      ConvertImage(Image, ifDefault);

    if (Angle > 45) and (Angle <= 135) then
    begin
      RotateMul90(Image, 90);
      Angle := Angle - 90;
    end
    else if (Angle > 135) and (Angle <= 225) then
    begin
      RotateMul90(Image, 180);
      Angle := Angle - 180;
    end
    else if (Angle > 225) and (Angle <= 315) then
    begin
      RotateMul90(Image, 270);
      Angle := Angle - 270;
    end;

    if Angle <> 0 then
      Rotate45(Image, Angle);

    if OldFmt <> Image.Format then
      ConvertImage(Image, OldFmt);
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorRotateImage, [ImageToStr(Image), Angle]);
  end;
end;

procedure RotateImageMul90(var Image: TImageData; AngleDeg: Integer);
var
  RotImage: TImageData;
  X, Y, BytesPerPixel: Integer;
  RotPix, Pix: PByte;
begin
  if TestImage(Image) then
  try
    InitImage(RotImage);

    while AngleDeg >= 360 do
      AngleDeg := AngleDeg - 360;
    while AngleDeg < 0 do
      AngleDeg := AngleDeg + 360;

    if (AngleDeg = 0) or (Abs(AngleDeg) = 360) then
      Exit;

    if not ((AngleDeg mod 90) = 0) then
      raise EImagingError.CreateFmt('Angle must be multiple of 90 but was: %d', [AngleDeg]);

    if ((AngleDeg = 90) or (AngleDeg = 270)) and (Image.Width <> Image.Height) then
      NewImage(Image.Height, Image.Width, Image.Format, RotImage)
    else
      NewImage(Image.Width, Image.Height, Image.Format, RotImage);

    BytesPerPixel := ImageFormatInfos[Image.Format].BytesPerPixel;

    RotPix := RotImage.Bits;
    case AngleDeg of
      90:
        begin
          for Y := 0 to RotImage.Height - 1 do
          begin
            Pix := @PByteArray(Image.Bits)[(Image.Width - Y - 1) * BytesPerPixel];
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Inc(Pix, Image.Width * BytesPerPixel);
            end;
          end;
        end;
      180:
        begin
          Pix := @PByteArray(Image.Bits)[((Image.Height - 1) * Image.Width +
            (Image.Width - 1)) * BytesPerPixel];
          for Y := 0 to RotImage.Height - 1 do
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Dec(Pix, BytesPerPixel);
            end;
        end;
      270:
        begin
          for Y := 0 to RotImage.Height - 1 do
          begin
            Pix := @PByteArray(Image.Bits)[((Image.Height - 1) * Image.Width + Y) * BytesPerPixel];
            for X := 0 to RotImage.Width - 1 do
            begin
              CopyPixel(Pix, RotPix, BytesPerPixel);
              Inc(RotPix, BytesPerPixel);
              Dec(Pix, Image.Width * BytesPerPixel);
            end;
          end;
        end;
    end;

    FreeMemNil(Image.Bits);
    RotImage.Palette := Image.Palette;
    Image := RotImage;
  except
    raise UpdateExceptMessage(GetExceptObject, 'Error while rotating image %s by %d degrees',
      [ImageToStr(Image), AngleDeg]);
  end;
end;

{ Drawing/Pixel functions }

function CopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean;
var
  Info: PImageFormatInfo;
  I, SrcWidthBytes, DstWidthBytes, MoveBytes: LongInt;
  SrcPointer, DstPointer: PByte;
  WorkImage: TImageData;
  OldFormat: TImageFormat;
begin
  Result := False;
  OldFormat := ifUnknown;
  if TestImage(SrcImage) and TestImage(DstImage) then
  try
    // Make sure we are still copying image to image, not invalid pointer to protected memory
    ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, SrcImage.Width, SrcImage.Height,
      Rect(0, 0, DstImage.Width, DstImage.Height));

    if (Width > 0) and (Height > 0) then
    begin
      Info := ImageFormatInfos[DstImage.Format];
      if Info.IsSpecial then
      begin
        // If dest image is in special format we convert it to default
        OldFormat := Info.Format;
        ConvertImage(DstImage, ifDefault);
        Info := ImageFormatInfos[DstImage.Format];
      end;
      if SrcImage.Format <> DstImage.Format then
      begin
        // If images are in different format source is converted to dest's format
        InitImage(WorkImage);
        CloneImage(SrcImage, WorkImage);
        ConvertImage(WorkImage, DstImage.Format);
      end
      else
        WorkImage := SrcImage;

      MoveBytes := Width * Info.BytesPerPixel;
      DstWidthBytes := DstImage.Width * Info.BytesPerPixel;
      DstPointer := @PByteArray(DstImage.Bits)[DstY * DstWidthBytes +
        DstX * Info.BytesPerPixel];
      SrcWidthBytes := WorkImage.Width * Info.BytesPerPixel;
      SrcPointer := @PByteArray(WorkImage.Bits)[SrcY * SrcWidthBytes +
        SrcX * Info.BytesPerPixel];

      for I := 0 to Height - 1 do
      begin
        Move(SrcPointer^, DstPointer^, MoveBytes);
        Inc(SrcPointer, SrcWidthBytes);
        Inc(DstPointer, DstWidthBytes);
      end;
      // If dest image was in special format we convert it back
      if OldFormat <> ifUnknown then
        ConvertImage(DstImage, OldFormat);
      // Working image must be freed if it is not the same as source image
      if WorkImage.Bits <> SrcImage.Bits then
        FreeImage(WorkImage);

      Result := True;
    end;
  except
    RaiseImaging(SErrorCopyRect, [ImageToStr(SrcImage), ImageToStr(DstImage)]);
  end;
end;

function FillRect(var Image: TImageData; X, Y, Width, Height: LongInt;
  FillColor: Pointer): Boolean;
var
  Info: PImageFormatInfo;
  I, J, ImageWidthBytes, RectWidthBytes, Bpp: Longint;
  LinePointer, PixPointer: PByte;
  OldFmt: TImageFormat;
begin
  Result := False;
  if TestImage(Image) then
  try
    ClipRectBounds(X, Y, Width, Height, Rect(0, 0, Image.Width, Image.Height));

    if (Width > 0) and (Height > 0) then
    begin
      OldFmt := Image.Format;
      if ImageFormatInfos[OldFmt].IsSpecial then
        ConvertImage(Image, ifDefault);

      Info := ImageFormatInfos[Image.Format];
      Bpp := Info.BytesPerPixel;
      ImageWidthBytes := Image.Width * Bpp;
      RectWidthBytes := Width * Bpp;
      LinePointer := @PByteArray(Image.Bits)[Y * ImageWidthBytes + X * Bpp];

      for I := 0 to Height - 1 do
      begin
        case Bpp of
          1: FillMemoryByte(LinePointer, RectWidthBytes, PByte(FillColor)^);
          2: FillMemoryWord(LinePointer, RectWidthBytes, PWord(FillColor)^);
          4: FillMemoryUInt32(LinePointer, RectWidthBytes, PUInt32(FillColor)^);
        else
          PixPointer := LinePointer;
          for J := 0 to Width - 1 do
          begin
            CopyPixel(FillColor, PixPointer, Bpp);
            Inc(PixPointer, Bpp);
          end;
        end;
        Inc(LinePointer, ImageWidthBytes);
      end;

      if OldFmt <> Image.Format then
        ConvertImage(Image, OldFmt);
    end;

    Result := True;
  except
    RaiseImaging(SErrorFillRect, [X, Y, Width, Height, ImageToStr(Image)]);
  end;
end;

function ReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldColor, NewColor: Pointer): Boolean;
var
  Info: PImageFormatInfo;
  I, J, WidthBytes, Bpp: Longint;
  LinePointer, PixPointer: PByte;
  OldFmt: TImageFormat;
begin
  Assert((OldColor <> nil) and (NewColor <> nil));
  Result := False;
  if TestImage(Image) then
  try
    ClipRectBounds(X, Y, Width, Height, Rect(0, 0, Image.Width, Image.Height));

    if (Width > 0) and (Height > 0) then
    begin
      OldFmt := Image.Format;
      if ImageFormatInfos[OldFmt].IsSpecial then
        ConvertImage(Image, ifDefault);

      Info := ImageFormatInfos[Image.Format];
      Bpp := Info.BytesPerPixel;
      WidthBytes := Image.Width * Bpp;
      LinePointer := @PByteArray(Image.Bits)[Y * WidthBytes + X * Bpp];

      for I := 0 to Height - 1 do
      begin
        PixPointer := LinePointer;
        for J := 0 to Width - 1 do
        begin
          if ComparePixels(PixPointer, OldColor, Bpp) then
            CopyPixel(NewColor, PixPointer, Bpp);
          Inc(PixPointer, Bpp);
        end;
        Inc(LinePointer, WidthBytes);
      end;

      if OldFmt <> Image.Format then
        ConvertImage(Image, OldFmt);
    end;

    Result := True;
  except
    RaiseImaging(SErrorReplaceColor, [X, Y, Width, Height, ImageToStr(Image)]);
  end;
end;

function StretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean;
var
  Info: PImageFormatInfo;
  WorkImage: TImageData;
  OldFormat: TImageFormat;
  Resampling: TSamplingFilter;
begin
  Result := False;
  OldFormat := ifUnknown;
  if TestImage(SrcImage) and TestImage(DstImage) then
  try
    // Make sure we are still copying image to image, not invalid pointer to protected memory
    ClipStretchBounds(SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY, DstWidth, DstHeight,
      SrcImage.Width, SrcImage.Height, Rect(0, 0, DstImage.Width, DstImage.Height));

    if (SrcWidth = DstWidth) and (SrcHeight = DstHeight) then
    begin
      // If source and dest rectangles have the same size call CopyRect
      Result := CopyRect(SrcImage, SrcX, SrcY, SrcWidth, SrcHeight, DstImage, DstX, DstY);
    end
    else if (SrcWidth > 0) and (SrcHeight > 0) and (DstWidth > 0) and (DstHeight > 0) then
    begin
      // If source and dest rectangles don't have the same size we do stretch
      Info := ImageFormatInfos[DstImage.Format];

      if Info.IsSpecial then
      begin
        // If dest image is in special format we convert it to default
        OldFormat := Info.Format;
        ConvertImage(DstImage, ifDefault);
        Info := ImageFormatInfos[DstImage.Format];
      end;

      if SrcImage.Format <> DstImage.Format then
      begin
        // If images are in different format source is converted to dest's format
        InitImage(WorkImage);
        CloneImage(SrcImage, WorkImage);
        ConvertImage(WorkImage, DstImage.Format);
      end
      else
        WorkImage := SrcImage;

      // Only pixel resize is supported for indexed images
      if Info.IsIndexed then
        Filter := rfNearest;

      if Filter = rfNearest then
      begin
        StretchNearest(WorkImage, SrcX, SrcY, SrcWidth, SrcHeight,
          DstImage, DstX, DstY, DstWidth, DstHeight);
      end
      else
      begin
        Resampling := sfNearest;
        case Filter of
          rfBilinear: Resampling := sfLinear;
          rfBicubic:  Resampling := DefaultCubicFilter;
          rfLanczos:  Resampling := sfLanczos;
        end;
        StretchResample(WorkImage, SrcX, SrcY, SrcWidth, SrcHeight,
          DstImage, DstX, DstY, DstWidth, DstHeight, Resampling);
      end;

      // If dest image was in special format we convert it back
      if OldFormat <> ifUnknown then
        ConvertImage(DstImage, OldFormat);
      // Working image must be freed if it is not the same as source image
      if WorkImage.Bits <> SrcImage.Bits then
        FreeImage(WorkImage);

      Result := True;
    end;
  except
    RaiseImaging(SErrorStretchRect, [ImageToStr(SrcImage), ImageToStr(DstImage)]);
  end;
end;

procedure GetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
var
  BytesPerPixel: LongInt;
begin
  Assert(Pixel <> nil);
  BytesPerPixel := ImageFormatInfos[Image.Format].BytesPerPixel;
  CopyPixel(@PByteArray(Image.Bits)[(Y * Image.Width + X) * BytesPerPixel],
    Pixel, BytesPerPixel);
end;

procedure SetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
var
  BytesPerPixel: LongInt;
begin
  Assert(Pixel <> nil);
  BytesPerPixel := ImageFormatInfos[Image.Format].BytesPerPixel;
  CopyPixel(Pixel, @PByteArray(Image.Bits)[(Y * Image.Width + X) * BytesPerPixel],
    BytesPerPixel);
end;

function GetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec;
var
  Info: PImageFormatInfo;
  Data: PByte;
begin
  Info := ImageFormatInfos[Image.Format];
  Data := @PByteArray(Image.Bits)[(Y * Image.Width + X) * Info.BytesPerPixel];
  Result := GetPixel32Generic(Data, Info, Image.Palette);
end;

procedure SetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec);
var
  Info: PImageFormatInfo;
  Data: PByte;
begin
  Info := ImageFormatInfos[Image.Format];
  Data := @PByteArray(Image.Bits)[(Y * Image.Width + X) * Info.BytesPerPixel];
  SetPixel32Generic(Data, Info, Image.Palette, Color);
end;

function GetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec;
var
  Info: PImageFormatInfo;
  Data: PByte;
begin
  Info := ImageFormatInfos[Image.Format];
  Data := @PByteArray(Image.Bits)[(Y * Image.Width + X) * Info.BytesPerPixel];
  Result := GetPixelFPGeneric(Data, Info, Image.Palette);
end;

procedure SetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec);
var
  Info: PImageFormatInfo;
  Data: PByte;
begin
  Info := ImageFormatInfos[Image.Format];
  Data := @PByteArray(Image.Bits)[(Y * Image.Width + X) * Info.BytesPerPixel];
  SetPixelFPGeneric(Data, Info, Image.Palette, Color);
end;

{ Palette Functions }

procedure NewPalette(Entries: LongInt; var Pal: PPalette32);
begin
  Assert((Entries > 2) and (Entries <= 65535));
  try
    GetMem(Pal, Entries * SizeOf(TColor32Rec));
    FillChar(Pal^, Entries * SizeOf(TColor32Rec), $FF);
  except
    RaiseImaging(SErrorNewPalette, [Entries]);
  end;
end;

procedure FreePalette(var Pal: PPalette32);
begin
  try
    FreeMemNil(Pal);
  except
    RaiseImaging(SErrorFreePalette, [Pal]);
  end;
end;

procedure CopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt);
begin
  Assert((SrcPal <> nil) and (DstPal <> nil));
  Assert((SrcIdx >= 0) and (DstIdx >= 0) and (Count >= 0));
  try
    Move(SrcPal[SrcIdx], DstPal[DstIdx], Count * SizeOf(TColor32Rec));
  except
    RaiseImaging(SErrorCopyPalette, [Count, SrcPal, DstPal]);
  end;
end;

function FindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32):
  LongInt;
var
  Col: TColor32Rec;
  I, MinDif, Dif: LongInt;
begin
  Assert(Pal <> nil);
  Result := -1;
  Col.Color := Color;
  try
    // First try to find exact match
    for I := 0 to Entries - 1 do
    with Pal[I] do
    begin
      if (A = Col.A) and (R = Col.R) and
        (G = Col.G) and (B = Col.B) then
      begin
        Result := I;
        Exit;
      end;
    end;

    // If exact match was not found, find nearest color
    MinDif := 1020;
    for I := 0 to Entries - 1 do
    with Pal[I] do
    begin
      Dif := Abs(R - Col.R);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(G - Col.G);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(B - Col.B);
      if Dif > MinDif then Continue;
      Dif := Dif + Abs(A - Col.A);
      if Dif < MinDif then
      begin
        MinDif := Dif;
        Result := I;
      end;
    end;
  except
    RaiseImaging(SErrorFindColor, [Pal, Entries]);
  end;
end;

procedure FillGrayscalePalette(Pal: PPalette32; Entries: LongInt);
var
  I: LongInt;
begin
  Assert(Pal <> nil);
  try
    for I := 0 to Entries - 1 do
    with Pal[I] do
    begin
      A := $FF;
      R := Byte(I);
      G := Byte(I);
      B := Byte(I);
    end;
  except
    RaiseImaging(SErrorGrayscalePalette, [Pal, Entries]);
  end;
end;

procedure FillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte = $FF);
var
  I, TotalBits, MaxEntries: LongInt;
begin
  Assert(Pal <> nil);
  TotalBits := RBits + GBits + BBits;
  MaxEntries := Min(Pow2Int(TotalBits), Entries);
  FillChar(Pal^, Entries * SizeOf(TColor32Rec), 0);
  try
    for I := 0 to MaxEntries - 1 do
    with Pal[I] do
    begin
      A := Alpha;
      if RBits > 0 then
        R := ((I shr Max(0, GBits + BBits - 1)) and (1 shl RBits - 1)) * 255 div (1 shl RBits - 1);
      if GBits > 0 then
        G := ((I shr Max(0, BBits - 1)) and (1 shl GBits - 1)) * 255 div (1 shl GBits - 1);
      if BBits > 0 then
        B := ((I shr 0) and (1 shl BBits - 1)) * 255 div (1 shl BBits - 1);
    end;
  except
    RaiseImaging(SErrorCustomPalette, [Pal, Entries]);
  end;
end;

procedure SwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel,
  DstChannel: LongInt);
var
  I: LongInt;
  Swap: Byte;
begin
  Assert(Pal <> nil);
  Assert((SrcChannel in [0..3]) and (DstChannel in [0..3]));
  try
    for I := 0 to Entries - 1 do
    with Pal[I] do
    begin
      Swap := Channels[SrcChannel];
      Channels[SrcChannel] := Channels[DstChannel];
      Channels[DstChannel] := Swap;
    end;
  except
    RaiseImaging(SErrorSwapPalette, [Pal, Entries]);
  end;
end;

{ Options Functions }

function SetOption(OptionId, Value: LongInt): Boolean;
begin
  Result := False;
  if (OptionId >= 0) and (OptionId < Length(Options)) and
    (Options[OptionID] <> nil) then
  begin
    Options[OptionID]^ := CheckOptionValue(OptionId, Value);
    Result := True;
  end;
end;

function GetOption(OptionId: LongInt): LongInt;
begin
  Result := InvalidOption;
  if (OptionId >= 0) and (OptionId < Length(Options)) and
    (Options[OptionID] <> nil) then
  begin
    Result := Options[OptionID]^;
  end;
end;

function PushOptions: Boolean;
begin
  Result := OptionStack.Push;
end;

function PopOptions: Boolean;
begin
  Result := OptionStack.Pop;
end;

{ Image Format Functions }

function GetImageFormatInfo(Format: TImageFormat; out Info: TImageFormatInfo): Boolean;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ImageFormatInfos[Format] <> nil then
  begin
    Info := ImageFormatInfos[Format]^;
    Result := True;
  end
  else
    Result := False;
end;

function GetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  if ImageFormatInfos[Format] <> nil then
    Result := ImageFormatInfos[Format].GetPixelsSize(Format, Width, Height)
  else
    Result := 0;
end;

{ IO Functions }

procedure SetUserFileIO(OpenProc: TOpenProc;
  CloseProc: TCloseProc; EofProc: TEofProc; SeekProc: TSeekProc; TellProc:
  TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc);
begin
  FileIO.Open := OpenProc;
  FileIO.Close := CloseProc;
  FileIO.Eof := EofProc;
  FileIO.Seek := SeekProc;
  FileIO.Tell := TellProc;
  FileIO.Read := ReadProc;
  FileIO.Write := WriteProc;
end;

procedure ResetFileIO;
begin
  FileIO := OriginalFileIO;
end;

{ Raw Image IO Functions }

procedure ReadRawImage(Handle: TImagingHandle;  Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset, RowLength: Integer);
var
  WidthBytes, I: Integer;
  Info: PImageFormatInfo;
begin
  Info := ImageFormatInfos[Format];
  // Calc scanline size
  WidthBytes := Info.GetPixelsSize(Format, Width, 1);
  if RowLength = 0 then
    RowLength := WidthBytes;
  // Create new image if needed - don't need to allocate new one if there is already
  // one with desired size and format
  if (Image.Width <> Width) or (Image.Height <> Height) or (Image.Format <> Format) then
    NewImage(Width, Height, Format, Image);
  // Move past the header
  IO.Seek(Handle, Offset, smFromCurrent);
  // Read scanlines from input
  for I := 0 to Height - 1 do
  begin
    IO.Read(Handle, @PByteArray(Image.Bits)[I * WidthBytes], WidthBytes);
    IO.Seek(Handle, RowLength - WidthBytes, smFromCurrent);
  end;
end;

procedure ReadRawImageFromFile(const FileName: string; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
begin
  Assert(FileName <> '');
  // Set IO ops to file ops and open given file
  SetFileIO;
  Handle := IO.Open(PChar(FileName), omReadOnly);
  try
    ReadRawImage(Handle, Width, Height, Format, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure ReadRawImageFromStream(Stream: TStream; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
begin
  Assert(Stream <> nil);
  if Stream.Size - Stream.Position = 0 then
    RaiseImaging(SErrorEmptyStream, []);
  // Set IO ops to stream ops and open given stream
  SetStreamIO;
  Handle := IO.Open(Pointer(Stream), omReadOnly);
  try
    ReadRawImage(Handle, Width, Height, Format, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure ReadRawImageFromMemory(Data: Pointer; DataSize: Integer; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
  MemRec: TMemoryIORec;
begin
  Assert((Data <> nil) and (DataSize > 0));
  // Set IO ops to memory ops and open given stream
  SetMemoryIO;
  MemRec := PrepareMemIO(Data, DataSize);
  Handle := IO.Open(@MemRec, omReadOnly);
  try
    ReadRawImage(Handle, Width, Height, Format, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure ReadRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  var Image: TImageData; Offset, RowLength: Integer);
var
  DestScanBytes, RectBytes, I: Integer;
  Info: PImageFormatInfo;
  Src, Dest: PByte;
begin
  Assert(Data <> nil);
  Assert((Left + Width <= Image.Width) and (Top + Height <= Image.Height));
  Info := ImageFormatInfos[Image.Format];

  // Calc scanline size
  DestScanBytes := Info.GetPixelsSize(Info.Format, Image.Width, 1);
  RectBytes := Info.GetPixelsSize(Info.Format, Width, 1);
  if RowLength = 0 then
    RowLength := RectBytes;

  Src := Data;
  Dest := @PByteArray(Image.Bits)[Top * DestScanBytes + Info.GetPixelsSize(Info.Format, Left, 1)];
  // Move past the header
  Inc(Src, Offset);

  // Read lines into rect in the existing image
  for I := 0 to Height - 1 do
  begin
    Move(Src^, Dest^, RectBytes);
    Inc(Src, RowLength);
    Inc(Dest, DestScanBytes);
  end;
end;

procedure WriteRawImage(Handle: TImagingHandle; const Image: TImageData;
  Offset, RowLength: Integer);
var
  WidthBytes, I: Integer;
  Info: PImageFormatInfo;
begin
  Info := ImageFormatInfos[Image.Format];
  // Calc scanline size
  WidthBytes := Info.GetPixelsSize(Image.Format, Image.Width, 1);
  if RowLength = 0 then
    RowLength := WidthBytes;
  // Move past the header
  IO.Seek(Handle, Offset, smFromCurrent);
  // Write scanlines to output
  for I := 0 to Image.Height - 1 do
  begin
    IO.Write(Handle, @PByteArray(Image.Bits)[I * WidthBytes], WidthBytes);
    IO.Seek(Handle, RowLength - WidthBytes, smFromCurrent);
  end;
end;

procedure WriteRawImageToFile(const FileName: string; const Image: TImageData;
  Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
begin
  Assert(FileName <> '');
  // Set IO ops to file ops and open given file
  SetFileIO;
  Handle := IO.Open(PChar(FileName), omCreate);
  try
    WriteRawImage(Handle, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure WriteRawImageToStream(Stream: TStream; const Image: TImageData;
  Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
begin
  Assert(Stream <> nil);
  // Set IO ops to stream ops and open given stream
  SetStreamIO;
  Handle := IO.Open(Pointer(Stream), omCreate);
  try
    WriteRawImage(Handle, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure WriteRawImageToMemory(Data: Pointer; DataSize: Integer; const Image: TImageData;
  Offset, RowLength: Integer);
var
  Handle: TImagingHandle;
  MemRec: TMemoryIORec;
begin
  Assert((Data <> nil) and (DataSize > 0));
  // Set IO ops to memory ops and open given stream
  SetMemoryIO;
  MemRec := PrepareMemIO(Data, DataSize);
  Handle := IO.Open(@MemRec, omCreate);
  try
    WriteRawImage(Handle, Image, Offset, RowLength);
  finally
    IO.Close(Handle);
  end;
end;

procedure WriteRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  const Image: TImageData; Offset, RowLength: Integer);
var
  SrcScanBytes, RectBytes, I: Integer;
  Info: PImageFormatInfo;
  Src, Dest: PByte;
begin
  Assert(Data <> nil);
  Assert((Left + Width <= Image.Width) and (Top + Height <= Image.Height));
  Info := ImageFormatInfos[Image.Format];

  // Calc scanline size
  SrcScanBytes := Info.GetPixelsSize(Info.Format, Image.Width, 1);
  RectBytes := Info.GetPixelsSize(Info.Format, Width, 1);
  if RowLength = 0 then
    RowLength := RectBytes;

  Src := @PByteArray(Image.Bits)[Top * SrcScanBytes + Info.GetPixelsSize(Info.Format, Left, 1)];
  Dest := Data;
  // Move past the header
  Inc(Dest, Offset);

  // Write lines from rect of the existing image
  for I := 0 to Height - 1 do
  begin
    Move(Src^, Dest^, RectBytes);
    Inc(Dest, RowLength);
    Inc(Src, SrcScanBytes);
  end;
end;

{ Convenience/helper Functions }

procedure ResizeImageToFit(const SrcImage: TImageData; FitWidth, FitHeight: Integer;
  Filter: TResizeFilter; var DestImage: TImageData);
var
  CurSize, FitSize, DestSize: TSize;
begin
  if not TestImage(SrcImage) then
    raise EImagingError.Create(SErrorInvalidInputImage);

  FitSize.CX := FitWidth;
  FitSize.CY := FitHeight;
  CurSize.CX := SrcImage.Width;
  CurSize.CY := SrcImage.Height;
  DestSize := ImagingUtility.ScaleSizeToFit(CurSize, FitSize);

  NewImage(Max(DestSize.CX, 1), Max(DestSize.CY, 1), SrcImage.Format, DestImage);
  if SrcImage.Palette <> nil then
    CopyPalette(SrcImage.Palette, DestImage.Palette, 0, 0, ImageFormatInfos[SrcImage.Format].PaletteEntries);

  StretchRect(SrcImage, 0, 0, CurSize.CX, CurSize.CY, DestImage, 0, 0,
    DestSize.CX, DestSize.CY, Filter);
end;

{ Color constructor functions }

function Color24(R, G, B: Byte): TColor24Rec;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function Color32(A, R, G, B: Byte): TColor32Rec;
begin
  Result.A := A;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function Color48(R, G, B: Word): TColor48Rec;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function Color64(A, R, G, B: Word): TColor64Rec;
begin
  Result.A := A;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function ColorFP(A, R, G, B: Single): TColorFPRec;
begin
  Result.A := A;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function ColorHF(A, R, G, B: THalfFloat): TColorHFRec;
begin
  Result.A := A;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function GetAlphaValue(Color32: TColor32): Byte;
begin
  Result := Color32 shr 24;
end;

function GetRedValue(Color32: TColor32): Byte;
begin
  Result := (Color32 shr 16) and $FF;
end;

function GetGreenValue(Color32: TColor32): Byte;
begin
  Result := (Color32 shr 8) and $FF;
end;

function GetBlueValue(Color32: TColor32): Byte;
begin
  Result := Color32 and $FF;
end;

{ ------------------------------------------------------------------------
                           Other Imaging Stuff
  ------------------------------------------------------------------------}

function GetFormatName(Format: TImageFormat): string;
begin
  if ImageFormatInfos[Format] <> nil then
    Result := ImageFormatInfos[Format].Name
  else
    Result := SUnknownFormat;
end;

function ImageToStr(const Image: TImageData): string;
var
  ImgSize: Integer;
begin
  if TestImage(Image) then
  with Image do
  begin
    ImgSize := Size;
    if ImgSize > 8192 then
      ImgSize := ImgSize div 1024;
    Result := SysUtils.Format(SImageInfo, [@Image, Width, Height,
      GetFormatName(Format), ImgSize + 0.0, Iff(ImgSize = Size, 'B', 'KiB'), Bits,
      Palette]);
  end
  else
    Result := SysUtils.Format(SImageInfoInvalid, [@Image]);
end;

function GetVersionStr: string;
begin
  Result := Format('%.1d.%.2d', [ImagingVersionMajor, ImagingVersionMinor]);
end;

function IffFormat(Condition: Boolean; const TruePart, FalsePart: TImageFormat): TImageFormat;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

procedure RegisterImageFileFormat(AClass: TImageFileFormatClass);
begin
  Assert(AClass <> nil);
  if ImageFileFormats = nil then
    ImageFileFormats := TList.Create;
  if GlobalMetadata = nil then
    GlobalMetadata := TMetadata.Create;
  if ImageFileFormats <> nil then
    ImageFileFormats.Add(AClass.Create);
end;

function RegisterOption(OptionId: LongInt; Variable: PLongInt): Boolean;
begin
  Result := False;
  if Options = nil then
    InitOptions;

  Assert(Variable <> nil);

  if OptionId >= Length(Options) then
    SetLength(Options, OptionId + InitialOptions);
  if (OptionId >= 0) and (OptionId < Length(Options)) {and (Options[OptionId] = nil) - must be able to override existing } then
  begin
    Options[OptionId] := Variable;
    Result := True;
  end;
end;

function FindImageFileFormatByExt(const Ext: string): TImageFileFormat;
var
  I: LongInt;
  SearchedExt: string;
begin
  Result := nil;
  SearchedExt := TrimLeftSet(Ext, ['.']);

  for I := ImageFileFormats.Count - 1 downto 0 do
    if TImageFileFormat(ImageFileFormats[I]).Extensions.IndexOf(SearchedExt) >= 0 then
    begin
      Result := TImageFileFormat(ImageFileFormats[I]);
      Exit;
    end;
end;

function FindImageFileFormatByName(const FileName: string): TImageFileFormat;
var
  I: LongInt;
begin
  Result := nil;

  if FileName = '' then
    Exit;

  for I := ImageFileFormats.Count - 1 downto 0 do
    if TImageFileFormat(ImageFileFormats[I]).TestFileName(FileName) then
    begin
      Result := TImageFileFormat(ImageFileFormats[I]);
      Exit;
    end;
end;

function FindImageFileFormatByClass(AClass: TImageFileFormatClass): TImageFileFormat;
var
  I: LongInt;
begin
  Result := nil;
  for I := 0 to ImageFileFormats.Count - 1 do
    if TImageFileFormat(ImageFileFormats[I]) is AClass then
    begin
      Result := TObject(ImageFileFormats[I]) as TImageFileFormat;
      Break;
    end;
end;

function GetFileFormatCount: LongInt;
begin
  Result := ImageFileFormats.Count;
end;

function GetFileFormatAtIndex(Index: LongInt): TImageFileFormat;
begin
  if (Index >= 0) and (Index < ImageFileFormats.Count) then
    Result := TImageFileFormat(ImageFileFormats[Index])
  else
    Result := nil;
end;

function GetImageFileFormatsFilter(OpenFileFilter: Boolean): string;
var
  I, J, Count: LongInt;
  Descriptions: string;
  Filters, CurFilter: string;
  FileFormat: TImageFileFormat;
begin
  Descriptions := '';
  Filters := '';
  Count := 0;

  for I := 0 to ImageFileFormats.Count - 1 do
  begin
    FileFormat := TObject(ImageFileFormats[I]) as TImageFileFormat;

    // If we are creating filter for save dialog and this format cannot save
    // files the we skip it
    if not OpenFileFilter and not FileFormat.CanSave then
      Continue;

    CurFilter := '';
    for J := 0 to FileFormat.Masks.Count - 1 do
    begin
      CurFilter := CurFilter + FileFormat.Masks[J];
      if J < FileFormat.Masks.Count - 1 then
        CurFilter := CurFilter + ';';
    end;

    FmtStr(Descriptions, '%s%s (%s)|%2:s', [Descriptions, FileFormat.Name, CurFilter]);
    if Filters <> '' then
      FmtStr(Filters, '%s;%s', [Filters, CurFilter])
    else
      Filters := CurFilter;

    if I < ImageFileFormats.Count - 1 then
        Descriptions := Descriptions + '|';

    Inc(Count);
  end;

  if (Count > 1) and OpenFileFilter then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [SAllFilter, Filters, Descriptions]);

  Result := Descriptions;
end;

function GetFilterIndexExtension(Index: LongInt; OpenFileFilter: Boolean): string;
var
  I, Count: LongInt;
  FileFormat: TImageFileFormat;
begin
  // -1 because filter indices are in 1..n range
  Index := Index - 1;
  Result := '';
  if OpenFileFilter then
  begin
    if Index > 0 then
      Index := Index - 1;
  end;

  if (Index >= 0) and (Index < ImageFileFormats.Count) then
  begin
    Count := 0;
    for I := 0 to ImageFileFormats.Count - 1 do
    begin
      FileFormat := TObject(ImageFileFormats[I]) as TImageFileFormat;
      if not OpenFileFilter and not FileFormat.CanSave then
        Continue;
      if Index = Count then
      begin
        if FileFormat.Extensions.Count > 0 then
          Result := FileFormat.Extensions[0];
        Exit;
      end;
      Inc(Count);
    end;
  end;
end;

function GetFileNameFilterIndex(const FileName: string; OpenFileFilter: Boolean): LongInt;
var
  I: LongInt;
  FileFormat: TImageFileFormat;
begin
  Result := 0;
  for I := 0 to ImageFileFormats.Count - 1 do
  begin
    FileFormat := TObject(ImageFileFormats[I]) as TImageFileFormat;
    if not OpenFileFilter and not FileFormat.CanSave then
      Continue;
    if FileFormat.TestFileName(FileName) then
    begin
      // +1 because filter indices are in 1..n range
      Inc(Result);
      if OpenFileFilter then
        Inc(Result);
      Exit;
    end;
    Inc(Result);
  end;
  Result := -1;
end;

function GetIO: TIOFunctions;
begin
  Result := IO;
end;

procedure RaiseImaging(const Msg: string; const Args: array of const);
var
  WholeMsg: string;
begin
  WholeMsg := Msg;
  if GetExceptObject <> nil then
  begin
    WholeMsg := WholeMsg + ' ' + SExceptMsg + ': ' +
      GetExceptObject.Message;
  end;
  raise EImagingError.CreateFmt(WholeMsg, Args);
end;

procedure RaiseImaging(const Msg: string);
begin
  RaiseImaging(Msg, []);
end;

{ Internal unit functions }

function CheckOptionValue(OptionId, Value: LongInt): LongInt;
begin
  case OptionId of
    ImagingColorReductionMask:
      Result := ClampInt(Value, 0, $FF);
    ImagingLoadOverrideFormat, ImagingSaveOverrideFormat:
      Result := Iff(ImagingFormats.IsImageFormatValid(TImageFormat(Value)),
        Value, LongInt(ifUnknown));
    ImagingMipMapFilter: Result := ClampInt(Value, Ord(Low(TSamplingFilter)),
        Ord(High(TSamplingFilter)));
  else
    Result := Value;
  end;
end;

procedure SetFileIO;
begin
  IO := FileIO;
end;

procedure SetStreamIO;
begin
  IO := StreamIO;
end;

procedure SetMemoryIO;
begin
  IO := MemoryIO;
end;

procedure InitImageFormats;
begin
  ImagingFormats.InitImageFormats(ImageFormatInfos);
end;

procedure FreeImageFileFormats;
var
  I: LongInt;
begin
  if ImageFileFormats <> nil then
    for I := 0 to ImageFileFormats.Count - 1 do
      TImageFileFormat(ImageFileFormats[I]).Free;
  FreeAndNil(ImageFileFormats);
end;

procedure InitOptions;
begin
  SetLength(Options, InitialOptions);
  OptionStack := TOptionStack.Create;
end;

procedure FreeOptions;
begin
  SetLength(Options, 0);
  FreeAndNil(OptionStack);
end;

{
  TImageFileFormat class implementation
}

constructor TImageFileFormat.Create(AMetadata: TMetadata);
begin
  inherited Create;
  FName := SUnknownFormat;
  FExtensions := TStringList.Create;
  FMasks := TStringList.Create;
  if AMetadata = nil then
    FMetadata := GlobalMetadata
  else
    FMetadata := AMetadata;
  Define;
end;

destructor TImageFileFormat.Destroy;
begin
  FExtensions.Free;
  FMasks.Free;
  inherited Destroy;
end;

procedure TImageFileFormat.Define;
begin
end;

function TImageFileFormat.PrepareLoad(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstFrame: Boolean): Boolean;
begin
  FMetadata.ClearMetaItems; // Clear old metadata
  FreeImagesInArray(Images);
  SetLength(Images, 0);
  Result := Handle <> nil;
end;

function TImageFileFormat.PostLoadCheck(var Images: TDynImageDataArray;
  LoadResult: Boolean): Boolean;
var
  I: LongInt;
begin
  if not LoadResult then
  begin
    FreeImagesInArray(Images);
    SetLength(Images, 0);
    Result := False;
  end
  else
  begin
    Result := (Length(Images) > 0) and TestImagesInArray(Images);

    if Result then
    begin
      // Convert to overridden format if it is set
      if LoadOverrideFormat <> ifUnknown then
        for I := Low(Images) to High(Images) do
          ConvertImage(Images[I], LoadOverrideFormat);
    end;
  end;
end;

function TImageFileFormat.PrepareSave(Handle: TImagingHandle;
  const Images: TDynImageDataArray; var Index: LongInt): Boolean;
var
  Len, I: LongInt;
begin
  CheckOptionsValidity;
  Result := False;
  if CanSave then
  begin
    Len := Length(Images);
    Assert(Len > 0);

    // If there are no images to be saved exit
    if Len = 0 then Exit;

    // Check index of image to be saved (-1 as index means save all images)
    if IsMultiImageFormat then
    begin
      if (Index >= Len) then
        Index := 0;

      if Index < 0 then
      begin
        Index := 0;
        FFirstIdx := 0;
        FLastIdx := Len - 1;
      end
      else
      begin
        FFirstIdx := Index;
        FLastIdx := Index;
      end;

      for I := FFirstIdx to FLastIdx - 1 do
      begin
        if not TestImage(Images[I]) then
          Exit;
      end;
    end
    else
    begin
      if (Index >= Len) or (Index < 0) then
        Index := 0;
      if not TestImage(Images[Index]) then
        Exit;
    end;

    Result := True;
  end;
end;

procedure TImageFileFormat.AddMasks(const AMasks: string);
var
  I: LongInt;
  Ext: string;
begin
  FExtensions.Clear;
  FMasks.CommaText := AMasks;
  FMasks.Delimiter := ';';

  for I := 0 to FMasks.Count - 1 do
  begin
    FMasks[I] := Trim(FMasks[I]);
    Ext := GetFileExt(FMasks[I]);
    if (Ext <> '') and (Ext <> '*') then
      FExtensions.Add(Ext);
  end;
end;

function TImageFileFormat.GetFormatInfo(Format: TImageFormat): TImageFormatInfo;
begin
  Result := ImageFormatInfos[Format]^;
end;

function TImageFileFormat.GetSupportedFormats: TImageFormats;
begin
  Result := FSupportedFormats;
end;

function TImageFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstFrame: Boolean): Boolean;
begin
  Result := False;
  RaiseImaging(SFileFormatCanNotLoad, [FName]);
end;

function TImageFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
begin
  Result := False;
  RaiseImaging(SFileFormatCanNotSave, [FName]);
end;

procedure TImageFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
end;

function TImageFileFormat.IsSupported(const Image: TImageData): Boolean;
begin
  Result := Image.Format in GetSupportedFormats;
end;

function TImageFileFormat.LoadFromFile(const FileName: string;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
begin
  Result := False;
  if CanLoad then
  try
    // Set IO ops to file ops and open given file
    SetFileIO;
    Handle := IO.Open(PChar(FileName), omReadOnly);
    try
      // Test if file contains valid image and if so then load it
      if TestFormat(Handle) then
      begin
        Result := PrepareLoad(Handle, Images, OnlyFirstLevel) and
          LoadData(Handle, Images, OnlyFirstLevel);
        Result := PostLoadCheck(Images, Result);
      end
      else
        RaiseImaging(SFileNotValid, [FileName, Name]);
    finally
      IO.Close(Handle);
    end;
  except
    RaiseImaging(SErrorLoadingFile, [FileName, FExtensions[0]]);
  end;
end;

function TImageFileFormat.LoadFromStream(Stream: TStream;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
  OldPosition: Int64;
begin
  Result := False;
  OldPosition := Stream.Position;
  if CanLoad then
  try
    // Set IO ops to stream ops and "open" given memory
    SetStreamIO;
    Handle := IO.Open(Pointer(Stream), omReadOnly);
    try
      // Test if stream contains valid image and if so then load it
      if TestFormat(Handle) then
      begin
        Result := PrepareLoad(Handle, Images, OnlyFirstLevel) and
          LoadData(Handle, Images, OnlyFirstLevel);
        Result := PostLoadCheck(Images, Result);
      end
      else
        RaiseImaging(SStreamNotValid, [@Stream, Name]);
    finally
      IO.Close(Handle);
    end;
  except
    Stream.Position := OldPosition;
    FreeImagesInArray(Images);
    RaiseImaging(SErrorLoadingStream, [@Stream, FExtensions[0]]);
  end;
end;

function TImageFileFormat.LoadFromMemory(Data: Pointer; Size: LongInt; var
  Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
  IORec: TMemoryIORec;
begin
  Result := False;
  if CanLoad then
  try
    // Set IO ops to memory ops and "open" given memory
    SetMemoryIO;
    IORec := PrepareMemIO(Data, Size);
    Handle := IO.Open(@IORec,omReadOnly);
    try
      // Test if memory contains valid image and if so then load it
      if TestFormat(Handle) then
      begin
        Result := PrepareLoad(Handle, Images, OnlyFirstLevel) and
          LoadData(Handle, Images, OnlyFirstLevel);
        Result := PostLoadCheck(Images, Result);
      end
      else
        RaiseImaging(SMemoryNotValid, [Data, Size, Name]);
    finally
      IO.Close(Handle);
    end;
  except
    RaiseImaging(SErrorLoadingMemory, [Data, Size, FExtensions[0]]);
  end;
end;

function TImageFileFormat.SaveToFile(const FileName: string;
  const Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
  Len, Index, I: LongInt;
  Ext, FName: string;
begin
  Result := False;
  if CanSave and TestImagesInArray(Images) then
  try
    SetFileIO;
    Len := Length(Images);
    if IsMultiImageFormat or
      (not IsMultiImageFormat and (OnlyFirstLevel or (Len = 1))) then
    begin
      Handle := IO.Open(PChar(FileName), GetSaveOpenMode);
      try
        if OnlyFirstLevel then
          Index := 0
        else
          Index := -1;
        // Write multi image to one file
        Result := PrepareSave(Handle, Images, Index) and SaveData(Handle, Images, Index);
      finally
        IO.Close(Handle);
      end;
    end
    else
    begin
      // Write multi image to file sequence
      Ext := ExtractFileExt(FileName);
      FName := ChangeFileExt(FileName, '');
      Result := True;
      for I := 0 to Len - 1 do
      begin
        Handle := IO.Open(PChar(Format(FName + '%.3d' + Ext, [I])), GetSaveOpenMode);
        try
          Index := I;
          Result := Result and PrepareSave(Handle, Images, Index) and
            SaveData(Handle, Images, Index);
          if not Result then
            Break;
        finally
          IO.Close(Handle);
        end;
      end;
    end;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorSavingFile, [FileName, FExtensions[0]]);
  end;
end;

function TImageFileFormat.SaveToStream(Stream: TStream;
  const Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
  Len, Index, I: LongInt;
  OldPosition: Int64;
begin
  Result := False;
  OldPosition := Stream.Position;
  if CanSave and TestImagesInArray(Images) then
  try
    SetStreamIO;
    Handle := IO.Open(PChar(Stream), GetSaveOpenMode);
    try
      if IsMultiImageFormat or OnlyFirstLevel then
      begin
        if OnlyFirstLevel then
          Index := 0
        else
          Index := -1;
        // Write multi image in one run
        Result := PrepareSave(Handle, Images, Index) and SaveData(Handle, Images, Index);
      end
      else
      begin
        // Write multi image to sequence
        Result := True;
        Len := Length(Images);
        for I := 0 to Len - 1 do
        begin
          Index := I;
          Result := Result and PrepareSave(Handle, Images, Index) and
            SaveData(Handle, Images, Index);
          if not Result then
            Break;
        end;
      end;
    finally
      IO.Close(Handle);
    end;
  except
    Stream.Position := OldPosition;
    raise UpdateExceptMessage(GetExceptObject, SErrorSavingStream, [@Stream, FExtensions[0]]);
  end;
end;

function TImageFileFormat.SaveToMemory(Data: Pointer; var Size: LongInt;
  const Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Handle: TImagingHandle;
  Len, Index, I: LongInt;
  IORec: TMemoryIORec;
begin
  Result := False;
  if CanSave and TestImagesInArray(Images) then
  try
    SetMemoryIO;
    IORec := PrepareMemIO(Data, Size);
    Handle := IO.Open(PChar(@IORec), GetSaveOpenMode);
    try
      if IsMultiImageFormat or OnlyFirstLevel then
      begin
        if OnlyFirstLevel then
          Index := 0
        else
          Index := -1;
        // Write multi image in one run
        Result := PrepareSave(Handle, Images, Index) and SaveData(Handle, Images, Index);
      end
      else
      begin
        // Write multi image to sequence
        Result := True;
        Len := Length(Images);
        for I := 0 to Len - 1 do
        begin
          Index := I;
          Result := Result and PrepareSave(Handle, Images, Index) and
            SaveData(Handle, Images, Index);
          if not Result then
            Break;
        end;
      end;
      Size := IORec.Position;
    finally
      IO.Close(Handle);
    end;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorSavingMemory, [Data, Size, FExtensions[0]]);
  end;
end;

function TImageFileFormat.MakeCompatible(const Image: TImageData;
  var Compatible: TImageData; out MustBeFreed: Boolean): Boolean;
begin
  InitImage(Compatible);

  if SaveOverrideFormat <> ifUnknown then
  begin
    // Save format override is active. Clone input and convert it to override format.
    CloneImage(Image, Compatible);
    ConvertImage(Compatible, SaveOverrideFormat);
    // Now check if override format is supported by file format. If it is not
    // then file format specific conversion (virtual method) is called.
    Result := IsSupported(Compatible);
    if not Result then
    begin
      ConvertToSupported(Compatible, GetFormatInfo(Compatible.Format));
      Result := IsSupported(Compatible);
    end;
  end     // Add IsCompatible function! not only checking by Format
  else if IsSupported(Image) then
  begin
    // No save format override and input is in format supported by this
    // file format. Just copy Image's fields to Compatible
    Compatible := Image;
    Result := True;
  end
  else
  begin
    // No override and input's format is not compatible with file format.
    // Clone it and the call file format specific conversion (virtual method).
    CloneImage(Image, Compatible);
    ConvertToSupported(Compatible, GetFormatInfo(Compatible.Format));
    Result := IsSupported(Compatible);
  end;
  // Tell the user that he must free Compatible after he's done with it
  // (if necessary).
  MustBeFreed := Image.Bits <> Compatible.Bits;
end;

function TImageFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
begin
  Result := False;
end;

function TImageFileFormat.TestFileName(const FileName: string): Boolean;
var
  I: LongInt;
  OnlyName: string;
begin
  OnlyName := ExtractFileName(FileName);
  // For each mask test if filename matches it 
  for I := 0 to FMasks.Count - 1 do
    if StrMaskMatch(OnlyName, FMasks[I], False) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TImageFileFormat.CheckOptionsValidity;
begin
end;

function TImageFileFormat.GetCanLoad: Boolean;
begin
  Result := ffLoad in FFeatures;
end;

function TImageFileFormat.GetCanSave: Boolean;
begin
  Result := ffSave in FFeatures;
end;

function TImageFileFormat.GetIsMultiImageFormat: Boolean;
begin
  Result := ffMultiImage in FFeatures;
end;

function TImageFileFormat.GetSaveOpenMode: TOpenMode;
begin
  // TODO: fix
  //if ffReadOnSave in FFeatures then
  //  Result := omReadWrite
  //else
    Result := omCreate;
end;

{ TOptionStack  class implementation }

constructor TOptionStack.Create;
begin
  inherited Create;
  FPosition := -1;
end;

destructor TOptionStack.Destroy;
var
  I: LongInt;
begin
  for I := 0 to OptionStackDepth - 1 do
    SetLength(FStack[I], 0);
  inherited Destroy;
end;

function TOptionStack.Pop: Boolean;
var
  I: LongInt;
begin
  Result := False;
  if FPosition >= 0  then
  begin
    SetLength(Options, Length(FStack[FPosition]));
    for I := 0 to Length(FStack[FPosition]) - 1 do
      if Options[I] <> nil then
        Options[I]^ := FStack[FPosition, I];
    Dec(FPosition);
    Result := True;
  end;
end;

function TOptionStack.Push: Boolean;
var
  I: LongInt;
begin
  Result := False;
  if FPosition < OptionStackDepth - 1 then
  begin
    Inc(FPosition);
    SetLength(FStack[FPosition], Length(Options));
    for I := 0 to Length(Options) - 1 do
      if Options[I] <> nil then
        FStack[FPosition, I] := Options[I]^;
    Result := True;
  end;
end;

{ TMetadata }

procedure TMetadata.SetMetaItem(const Id: string; const Value: Variant;
  ImageIndex: Integer);
begin
  AddMetaToList(FLoadMetaItems, Id, Value, ImageIndex);
end;

procedure TMetadata.SetMetaItemForSaving(const Id: string; const Value: Variant;
  ImageIndex: Integer);
begin
  AddMetaToList(FSaveMetaItems, Id, Value, ImageIndex);
end;

procedure TMetadata.AddMetaToList(List: TStringList; const Id: string;
  const Value: Variant; ImageIndex: Integer);
var
  Item: TMetadataItem;
  Idx: Integer;
  FullId: string;
begin
  FullId := GetMetaItemName(Id, ImageIndex);
  if List.Find(FullId, Idx) then
    (List.Objects[Idx] as TMetadataItem).Value := Value
  else
  begin
    Item := TMetadataItem.Create;
    Item.Id := Id;
    Item.ImageIndex := ImageIndex;
    Item.Value := Value;
    List.AddObject(FullId, Item);
  end;
end;

procedure TMetadata.ClearMetaItems;
begin
  ClearMetaList(FLoadMetaItems);
end;

procedure TMetadata.ClearMetaItemsForSaving;
begin
  ClearMetaList(FSaveMetaItems);
end;

procedure TMetadata.ClearMetaList(List: TStringList);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List.Objects[I].Free;
  List.Clear;
end;

procedure TMetadata.CopyLoadedMetaItemsForSaving;
var
  I: Integer;
  Copy, Orig: TMetadataItem;
begin
  ClearMetaItemsForSaving;
  for I := 0 to FLoadMetaItems.Count - 1 do
  begin
    Orig := TMetadataItem(FLoadMetaItems.Objects[I]);
    Copy := TMetadataItem.Create;
    Copy.Id := Orig.Id;
    Copy.ImageIndex := Orig.ImageIndex;
    Copy.Value := Orig.Value;
    FSaveMetaItems.AddObject(GetMetaItemName(Copy.Id, Copy.ImageIndex), Copy);
  end;
end;

constructor TMetadata.Create;
begin
  inherited;
  FLoadMetaItems := TStringList.Create;
  FLoadMetaItems.Sorted := True;
  FSaveMetaItems := TStringList.Create;
  FSaveMetaItems.Sorted := True;
end;

destructor TMetadata.Destroy;
begin
  ClearMetaItems;
  ClearMetaItemsForSaving;
  FLoadMetaItems.Free;
  FSaveMetaItems.Free;
  inherited;
end;

function TMetadata.GetMetaById(const Id: string): Variant;
var
  Idx: Integer;
begin
  if FLoadMetaItems.Find(Id, Idx) then
    Result := (FLoadMetaItems.Objects[Idx] as TMetadataItem).Value
  else
    Result := Variants.Null;
end;

function TMetadata.GetMetaByIdMulti(const Id: string; ImageIndex: Integer): Variant;
begin
  Result := GetMetaById(GetMetaItemName(Id, ImageIndex));
end;

function TMetadata.GetSaveMetaById(const Id: string): Variant;
var
  Idx: Integer;
begin
  if FSaveMetaItems.Find(Id, Idx) then
    Result := (FSaveMetaItems.Objects[Idx] as TMetadataItem).Value
  else
    Result := Variants.Null;
end;

function TMetadata.GetSaveMetaByIdMulti(const Id: string;
  ImageIndex: Integer): Variant;
begin
  Result := GetSaveMetaById(GetMetaItemName(Id, ImageIndex));
end;

function TMetadata.GetMetaByIdx(Index: Integer): TMetadataItem;
begin
  Result := FLoadMetaItems.Objects[Index] as TMetadataItem;
end;

function TMetadata.GetMetaCount: Integer;
begin
  Result := FLoadMetaItems.Count;
end;

function TMetadata.GetMetaItemName(const Id: string;
  ImageIndex: Integer): string;
begin
  Result := Iff(ImageIndex = 0, Id, Format(SMetaIdForSubImage, [Id, ImageIndex]));
end;

function TMetadata.GetPhysicalPixelSize(ResUnit: TResolutionUnit; out XSize,
  YSize: Double; MetaForSave: Boolean; ImageIndex: Integer): Boolean;
type
  TGetter = function(const Id: string; ImageIndex: Integer): Variant of object;
var
  Getter: TGetter;
  XMeta, YMeta: Variant;
begin
  if MetaForSave then
    Getter := GetSaveMetaByIdMulti
  else
    Getter := GetMetaByIdMulti;

  XMeta := Getter(SMetaPhysicalPixelSizeX, ImageIndex);
  YMeta := Getter(SMetaPhysicalPixelSizeY, ImageIndex);
  XSize := -1;
  YSize := -1;

  Result := not VarIsNull(XMeta) or not VarIsNull(YMeta);

  if not Result then
    Exit;

  if not VarIsNull(XMeta) then
    XSize := XMeta;
  if not VarIsNull(YMeta) then
    YSize := YMeta;

  if XSize < 0 then
    XSize := YSize;
  if YSize < 0 then
    YSize := XSize;

  TranslateUnits(ResUnit, XSize, YSize);
end;

procedure TMetadata.SetPhysicalPixelSize(ResUnit: TResolutionUnit; XSize,
  YSize: Double; MetaForSave: Boolean; ImageIndex: Integer);
type
  TAdder = procedure(const Id: string; const Value: Variant; ImageIndex: Integer) of object;
var
  Adder: TAdder;
begin
  TranslateUnits(ResUnit, XSize, YSize);

  if MetaForSave then
    Adder := SetMetaItemForSaving
  else
    Adder := SetMetaItem;

  Adder(SMetaPhysicalPixelSizeX, XSize, ImageIndex);
  Adder(SMetaPhysicalPixelSizeY, YSize, ImageIndex);
end;

procedure TMetadata.TranslateUnits(ResolutionUnit: TResolutionUnit; var XRes,
  YRes: Double);
var
  UnitSize: Double;
begin
  case ResolutionUnit of
    ruDpi: UnitSize := 25400;
    ruDpm: UnitSize := 1e06;
    ruDpcm: UnitSize := 1e04;
  else
    UnitSize := 1;
  end;
  if ResolutionUnit <> ruSizeInMicroMeters then
  begin
    XRes := UnitSize / XRes;
    YRes := UnitSize / YRes;
  end;
end;

function TMetadata.HasMetaItem(const Id: string; ImageIndex: Integer): Boolean;
begin
  Result := GetMetaByIdMulti(Id, ImageIndex) <> Variants.Null;
end;

function TMetadata.HasMetaItemForSaving(const Id: string; ImageIndex: Integer): Boolean;
begin
  Result := GetSaveMetaByIdMulti(Id, ImageIndex) <> Variants.Null;
end;

initialization
{$IFDEF MEMCHECK}
  {$IF CompilerVersion >= 18}
    System.ReportMemoryLeaksOnShutdown := True;
  {$IFEND}
{$ENDIF}
  if GlobalMetadata = nil then
    GlobalMetadata := TMetadata.Create;
  if ImageFileFormats = nil then
    ImageFileFormats := TList.Create;
  InitImageFormats;
  RegisterOption(ImagingColorReductionMask, @ColorReductionMask);
  RegisterOption(ImagingLoadOverrideFormat, @LoadOverrideFormat);
  RegisterOption(ImagingSaveOverrideFormat, @SaveOverrideFormat);
  RegisterOption(ImagingMipMapFilter, @MipMapFilter);
  RegisterOption(ImagingBinaryThreshold, @BinaryThreshold);
finalization
  FreeOptions;
  FreeImageFileFormats;
  GlobalMetadata.Free;

{
  File Notes (obsolete):

  -- 0.80 ------------------------------------------------------
    - Added new color records constructor functions (Color24(..), Color32(..)).
    - Added convenience channel getters for TColor32 (GetGreenValue, ...).

  -- 0.77.1 ---------------------------------------------------
    - Updated IO Open functions according to changes in ImagingTypes.
    - Fixed bug in SplitImage that could cause wrong size of edge chunks.
    - Metadata support fixes and extensions (frame delays, animation loops).

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Started reworking exception raising to keep the original class type
      (e.g. in NewImage EOutOfMemory could be raised but was hidden
      by EImagingError raised afterwards in NewImage try/except).
    - Fixed possible AV in Rotate45 subproc of RotateImage.
    - Added ReadRawXXX and WriteRawXXX functions for raw image bits IO.
    - Implemented ImagingBinaryThreshold option.
    - Added support for simple image metadata loading/saving.
    - Moved file format definition (name, exts, caps, ...) from
      constructor to new Define method.
    - Fixed some memory leaks caused by failures during image loading.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Extended RotateImage to allow arbitrary angle rotations.
    - Reversed the order file formats list is searched so
      if you register a new one it will be found sooner than
      built in formats.
    - Fixed memory leak in ResizeImage occurring when resizing
      indexed images.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added position/size checks to LoadFromStream functions.
    - Changed conditional compilation in impl. uses section to reflect changes
      in LINK symbols.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - GenerateMipMaps now generates all smaller levels from
      original big image (better results when using more advanced filters).
      Also conversion to compatible image format is now done here not
      in FillMipMapLevel (that is called for every mipmap level).

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - MakePaletteForImages now works correctly for indexed and special format images
    - Fixed bug in StretchRect: Image was not properly stretched if
      src and dst dimensions differed only in height.
    - ConvertImage now fills new image with zeroes to avoid random data in
      some conversions (RGB->XRGB)
    - Changed RegisterOption procedure to function
    - Changed bunch of palette functions from low level interface to procedure
      (there was no reason for them to be functions).
    - Changed FreeImage and FreeImagesInArray functions to procedures.
    - Added many assertions, come try-finally, other checks, and small code
      and doc changes.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - GenerateMipMaps threw failed assertion when input was indexed or special,
      fixed.
    - Added CheckOptionsValidity to TImageFileFormat and its descendants.
    - Unit ImagingExtras which registers file formats in Extras package
      is now automatically added to uses clause if LINK_EXTRAS symbol is
      defined in ImagingOptions.inc file.
    - Added EnumFileFormats function to low level interface.
    - Fixed bug in SwapChannels which could cause AV when swapping alpha
      channel of A8R8G8B8 images.
    - Converting loaded images to ImagingOverrideFormat is now done
      in PostLoadCheck method to avoid code duplicity.
    - Added GetFileFormatCount and GetFileFormatAtIndex functions
    - Bug in ConvertImage: if some format was converted to similar format
      only with swapped channels (R16G16B16<>B16G16R16) then channels were
      swapped correctly but new data format (swapped one) was not set.
    - Made TImageFileFormat.MakeCompatible public non-virtual method
      (and modified its function). Created new virtual
      ConvertToSupported which should be overridden by descendants.
      Main reason for doing this is to avoid duplicate code that was in all
      TImageFileFormat's descendants.
    - Changed TImageFileFormat.GetFormatInfo's result type to TImageFormatInfo.
    - Split overloaded FindImageFileFormat functions to
      FindImageFileFormatByClass and FindImageFileFormatByExt and created new
      FindImageFileFormatByName which operates on whole filenames.
    - Function GetExtensionFilterIndex renamed to GetFileNameFilterIndex
      (because it now works with filenames not extensions).
    - DetermineFileFormat now first searches by filename and if not found
      then by data.
    - Added TestFileName method to TImageFileFormat.
    - Updated GetImageFileFormatsFilter to uses Masks instead of Extensions
      property of TImageFileFormat. Also you can now request
      OpenDialog and SaveDialog type filters
    - Added Masks property and AddMasks method to TImageFileFormat.
      AddMasks replaces AddExtensions, it uses filename masks instead
      of some filename extensions to identify supported files.
    - Changed TImageFileFormat.LoadData procedure to function and
      moved various duplicate code from its descendants (check index,...)
      here to TImageFileFormat helper methods.
    - Changed TImageFileFormat.SaveData procedure to function and
      moved various duplicate code from its descendants (check index,...)
      here to TImageFileFormat helper methods.
    - Removed RAISE_EXCEPTIONS define, exceptions are now raised every time
    - Added MustBeFreed parameter to TImageFileFormat.MakeCompatible method
      that indicates that compatible image returned by this method must be
      freed after its usage.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - fixed bug in NewImage: if given format was ifDefault it wasn't
      replaced with DefaultImageFormat constant which caused problems later
      in other units 
    - fixed bug in RotateImage which caused that rotated special format
      images were whole black
    - LoadImageFromXXX and LoadMultiImageFromXXX now use DetermineXXXFormat
      when choosing proper loader, this eliminated need for Ext parameter
      in stream and memory loading functions
    - added GetVersionStr function
    - fixed bug in ResizeImage which caused indexed images to lose their
      palette during process resulting in whole black image
    - Clipping in ...Rect functions now uses clipping procs from ImagingUtility,
      it also works better
    - FillRect optimization for 8, 16, and 32 bit formats
    - added pixel set/get functions to low level interface:
      GetPixelDirect, SetPixelDirect, GetPixel32, SetPixel32,
      GetPixelFP, SetPixelFP
    - removed GetPixelBytes low level intf function - redundant
      (same data can be obtained by GetImageFormatInfo)
    - made small changes in many parts of library to compile
      on AMD64 CPU (Linux with FPC)
    - changed InitImage to procedure (function was pointless)
    - Method TestFormat of TImageFileFormat class made public
      (was protected)
    - added function IsFileFormatSupported to low level interface
      (contributed by Paul Michell)
    - fixed some missing format arguments from error strings
      which caused Format function to raise exception
    - removed forgotten debug code that disabled filtered resizing of images with
      channel bitcounts > 8

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - changed order of parameters of CopyRect function
    - GenerateMipMaps now filters mipmap levels
    - ResizeImage functions was extended to allow bilinear and bicubic filtering
    - added StretchRect function to low level interface
    - added functions GetImageFileFormatsFilter, GetFilterIndexExtension,
      and GetExtensionFilterIndex

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - added function RotateImage to low level interface
    - moved TImageFormatInfo record and types required by it to
      ImagingTypes unit, changed GetImageFormatInfo low level
      interface function to return TImageFormatInfo instead of short info
    - added checking of options values validity before they are used
    - fixed possible memory leak in CloneImage
    - added ReplaceColor function to low level interface
    - new function FindImageFileFormat by class added

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - added DetermineFileFormat, DetermineStreamFormat, DetermineMemoryFormat,
      GetPixelsSize functions to low level interface
    - added NewPalette, CopyPalette, FreePalette functions
      to low level interface
    - added MapImageToPalette, FillRect, SplitImage, MakePaletteForImages
      functions to low level interface
    - fixed buggy FillCustomPalette function (possible div by zero and others)
    - added CopyRect function to low level interface
    - Member functions of TImageFormatInfo record implemented for all formats
    - before saving images TestImagesInArray is called now
    - added TestImagesInArray function to low level interface
    - added GenerateMipMaps function to low level interface
    - stream position in load/save from/to stream is now set to position before
      function was called if error occurs
    - when error occurred during load/save from/to file file handle
      was not released
    - CloneImage returned always False

}
end.

