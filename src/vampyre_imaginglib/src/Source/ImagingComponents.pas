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

{ This unit contains VCL/LCL TGraphic descendant which uses Imaging library
  for saving and loading.}
unit ImagingComponents;

{$I ImagingOptions.inc}

interface

{$IF Defined(FPC) and Defined(LCL)}
  { Castle Game Engine undefined -- this breaks image loading in CGE editor,
    e.g. CGE logo.
    TODO: investigate, submit bug or PR upstream. }
  {.$DEFINE COMPONENT_SET_LCL}
{$ELSEIF Defined(DELPHI)}
  { Castle Game Engine undefined -- we want to support FMX too. }
  {.$DEFINE COMPONENT_SET_VCL}
{$IFEND}

{$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}
// If no component sets should be used just include empty unit.
implementation
{$ELSE}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Types, Classes,
{$IFDEF COMPONENT_SET_VCL}
  Graphics,
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  GraphType,
  Graphics,
  LCLType,
  LCLIntf,
{$ENDIF}
  ImagingTypes, Imaging, ImagingClasses;

type
  { Graphic class which uses Imaging to load images.
    It has standard TBitmap class as ancestor and it can
    Assign also to/from TImageData structures and TBaseImage
    classes. If you want to perfectly preserve the original pixel format
    of the source image then these classes may not for you.

    This class is automatically registered to TPicture for all
    file extensions supported by Imaging (useful only for loading).
    If you just want to load images in various formats you can use this
    class or simply use TPicture.LoadFromXXX which will create this class
    automatically.

    For saving it uses inherited TBitmap methods
    (it saves the image in BMP format regardless of extension you request).
    For TGraphic class that saves with Imaging look
    at TImagingGraphicForSave class.}
  TImagingGraphic = class(TBitmap)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    { Called by TFiler when reading and writing TPicture.Data property.
      We need to override ReadData+WriteData otherwise inherited ones from
      TBitmap would be called resulting in errors.}
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;

    { Loads new image from the stream. It can load all image
      file formats supported by Imaging (and enabled of course)
      even though it is called by descendant class capable of
      saving only one file format.}
    procedure LoadFromStream(Stream: TStream); override;
    { Copies the image contained in Source to this graphic object.
      Supports also TBaseImage descendants from ImagingClasses unit. }
    procedure Assign(Source: TPersistent); override;
    { Copies the image contained in TBaseImage to this graphic object.}
    procedure AssignFromImage(Image: TBaseImage);
    { Copies the current image to TBaseImage object.}
    procedure AssignToImage(Image: TBaseImage);
    { Copies the image contained in TImageData structure to this graphic object.}
    procedure AssignFromImageData(const ImageData: TImageData);
    { Copies the current image to TImageData structure.}
    procedure AssignToImageData(var ImageData: TImageData);

  {$IFDEF COMPONENT_SET_LCL}
    { Needed for TGraphic.LoadFromResourceName() to work.
      We return RT_RCDATA here. Also for TImagingBitmap since
      RT_BITMAP is stored differently than bitmap on disk (no BITMAPFILEHEADER).}
    function GetResourceType: TResourceType; override;
    { Used by TPicture.LoadFromStream to find the right TGraphic class for streams. }
    class function IsStreamFormatSupported(Stream: TStream): boolean; override;
  {$ENDIF}
  end;

  TImagingGraphicClass = class of TImagingGraphic;

  { Base (abstract) class for file format specific TGraphic classes that use
    Imaging for saving. Each descendant class can load all file formats
    supported by Imaging but save only one format (TImagingBitmap
    for *.bmp, TImagingJpeg for *.jpg). The image is saved in this one file
    format regardless of the extension you request).

    Format specific classes also allow easy access to Imaging options that
    affect saving of files (they are properties here).}
  TImagingGraphicForSave = class(TImagingGraphic)
  protected
    FDefaultFileExt: string;
    FSavingFormat: TImageFormat;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    { Saves the current image to the stream. It is saved in the
      file format according to the DefaultFileExt property.
      So each descendant class can save some other file format.}
    procedure SaveToStream(Stream: TStream); override;
    { Returns TImageFileFormat descendant for this graphic class.}
    class function GetFileFormat: TImageFileFormat; virtual; abstract;
  {$IFDEF COMPONENT_SET_LCL}
    { Returns file extensions of this graphic class.}
    class function GetFileExtensions: string; override;
    { Returns default MIME type of this graphic class.}
    function GetMimeType: string; override;
  {$ENDIF}
    { Default (the most common) file extension of this graphic class.}
    property DefaultFileExt: string read FDefaultFileExt;
  end;

  TImagingGraphicForSaveClass = class of TImagingGraphicForSave;

{$IFNDEF DONT_LINK_BITMAP}
  { TImagingGraphic descendant for loading/saving Windows bitmaps.
    VCL/LCL both have native support for bitmaps so you might
    want to disable this class (although you can save bitmaps with
    RLE compression with this class).}
  TImagingBitmap = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingBitmapRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}
  { TImagingGraphic descendant for loading/saving JPEG images.}
  TImagingJpeg = class(TImagingGraphicForSave)
  protected
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingJpegQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJpegProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_PNG}
  { TImagingGraphic descendant for loading/saving PNG images.}
  TImagingPNG = class(TImagingGraphicForSave)
  protected
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingPNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingPNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_GIF}
  { TImagingGraphic descendant for loading/saving GIF images.}
  TImagingGIF = class(TImagingGraphicForSave)
  public
    class function GetFileFormat: TImageFileFormat; override;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}
  { TImagingGraphic descendant for loading/saving Targa images.}
  TImagingTarga = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingTargaRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}
  { Compression type used when saving DDS files by TImagingDds.}
  TDDSCompression = (dcNone, dcDXT1, dcDXT3, dcDXT5);

  { TImagingGraphic descendant for loading/saving DDS images.}
  TImagingDDS = class(TImagingGraphicForSave)
  protected
    FCompression: TDDSCompression;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { You can choose compression type used when saving DDS file.
      dcNone means that file will be saved in the current bitmaps pixel format.}
    property Compression: TDDSCompression read FCompression write FCompression;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}
  { TImagingGraphic descendant for loading/saving MNG images.}
  TImagingMNG = class(TImagingGraphicForSave)
  protected
    FLossyCompression: Boolean;
    FLossyAlpha: Boolean;
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingMNGLossyCompression option for details.}
    property LossyCompression: Boolean read FLossyCompression write FLossyCompression;
    { See ImagingMNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingMNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingMNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
    { See ImagingMNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingMNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}
  { TImagingGraphic descendant for loading/saving JNG images.}
  TImagingJNG = class(TImagingGraphicForSave)
  protected
    FLossyAlpha: Boolean;
    FAlphaPreFilter: LongInt;
    FAlphaCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingJNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingJNGPreFilter option for details.}
    property AlphaPreFilter: LongInt read FAlphaPreFilter write FAlphaPreFilter;
    { See ImagingJNGCompressLevel option for details.}
    property AlphaCompressLevel: LongInt read FAlphaCompressLevel write FAlphaCompressLevel;
    { See ImagingJNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{ Returns bitmap pixel format with the closest match with given data format.}
function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
{ Returns data format with closest match with given bitmap pixel format.}
function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;

{ Converts TImageData structure to VCL/CLX/LCL bitmap.}
procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TImageData structure.}
procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
{ Converts TBaseImage instance to VCL/CLX/LCL bitmap.}
procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TBaseImage. Image must exist before
  procedure is called. It overwrites its current image data.
  When Image is TMultiImage only the current image level is overwritten.}
procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);

{ Displays image onto TCanvas to rectangle DstRect. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData); overload;
{ Displays image stored in TImageData structure onto TCanvas. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow). Dest and Src
  rectangles represent coordinates in the form (X1, Y1, X2, Y2).}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect); overload;
{ Displays image onto TCanvas at position [DstX, DstY]. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage); overload;
{ Displays image onto TCanvas to rectangle DstRect. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage); overload;
{ Displays part of the image specified by SrcRect onto TCanvas to rectangle DstRect.
  This procedure draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect); overload;

{$IFDEF MSWINDOWS}
{ Displays image stored in TImageData structure onto Windows device context.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$ENDIF}

procedure RegisterTypes;

implementation

uses
{$IF Defined(LCL)}
  {$IF Defined(LCLGTK2)}
    InterfaceBase, GLib2, GDK2, GTK2, GTK2Def, GTK2Proc,
  {$IFEND}
{$IFEND}
{$IFNDEF DONT_LINK_BITMAP}
  ImagingBitmap,
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  ImagingJpeg,
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  ImagingGif,
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  ImagingTarga,
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  ImagingDds,
{$ENDIF}
{$IF not Defined(DONT_LINK_PNG) or not Defined(DONT_LINK_MNG) or not Defined(DONT_LINK_JNG)}
  ImagingNetworkGraphics,
{$IFEND}
  ImagingFormats, ImagingUtility;

resourcestring
  SBadFormatDataToBitmap = 'Cannot find compatible bitmap format for image %s';
  SBadFormatBitmapToData = 'Cannot find compatible data format for bitmap %p';
  SBadFormatDisplay = 'Unsupported image format passed';
  SUnsupportedLCLWidgetSet = 'This function is not implemented for current LCL widget set';
  SImagingGraphicName = 'Imaging Graphic AllInOne';

var
  RegisteredFormats: TList;
  RegisteredGraphicsClasses: Boolean = False;

{ Registers types to VCL/LCL.
  In some cases (base+ext package installed in Lazarus) RegisterTypes can be
  called twice so must keep track of which formats were already registered. }
procedure RegisterTypes;
var
  I: LongInt;

  procedure RegisterFileFormatAllInOne(Format: TImageFileFormat);
  var
    I: LongInt;
  begin
    if RegisteredFormats.IndexOf(Format) >= 0 then
      Exit;

    for I := 0 to Format.Extensions.Count - 1 do
    begin
      TPicture.RegisterFileFormat(Format.Extensions[I], SImagingGraphicName,
        TImagingGraphic);
    end;
    RegisteredFormats.Add(Format);
  end;

  procedure RegisterFileFormat(AClass: TImagingGraphicForSaveClass);
  var
    I: LongInt;
  begin
    for I := 0 to AClass.GetFileFormat.Extensions.Count - 1 do
      TPicture.RegisterFileFormat(AClass.GetFileFormat.Extensions[I],
        AClass.GetFileFormat.Name, AClass);
  end;

begin
  for I := Imaging.GetFileFormatCount - 1 downto 0 do
    RegisterFileFormatAllInOne(Imaging.GetFileFormatAtIndex(I));
  Classes.RegisterClass(TImagingGraphic);

  if RegisteredGraphicsClasses then
    Exit;

{$IFNDEF DONT_LINK_TARGA}
  RegisterFileFormat(TImagingTarga);
  Classes.RegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  RegisterFileFormat(TImagingDDS);
  Classes.RegisterClass(TImagingDDS);
{$ENDIF}
{$IFNDEF DONT_LINK_JNG}
  RegisterFileFormat(TImagingJNG);
  Classes.RegisterClass(TImagingJNG);
{$ENDIF}
{$IFNDEF DONT_LINK_MNG}
  RegisterFileFormat(TImagingMNG);
  Classes.RegisterClass(TImagingMNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  RegisterFileFormat(TImagingGIF);
  Classes.RegisterClass(TImagingGIF);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  {$IFDEF COMPONENT_SET_LCL}
    // Unregister Lazarus default PNG loader which crashes on some PNG files
    TPicture.UnregisterGraphicClass(TPortableNetworkGraphic);
  {$ENDIF}
  RegisterFileFormat(TImagingPNG);
  Classes.RegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  RegisterFileFormat(TImagingJpeg);
  Classes.RegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_BITMAP}
  RegisterFileFormat(TImagingBitmap);
  Classes.RegisterClass(TImagingBitmap);
{$ENDIF}

  RegisteredGraphicsClasses := True;
end;

{ Unregisters types from VCL/LCL.}
procedure UnRegisterTypes;
begin
{$IFNDEF DONT_LINK_BITMAP}
  TPicture.UnregisterGraphicClass(TImagingBitmap);
  Classes.UnRegisterClass(TImagingBitmap);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  TPicture.UnregisterGraphicClass(TImagingJpeg);
  Classes.UnRegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  TPicture.UnregisterGraphicClass(TImagingPNG);
  Classes.UnRegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  TPicture.UnregisterGraphicClass(TImagingGIF);
  Classes.UnRegisterClass(TImagingGIF);
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  TPicture.UnregisterGraphicClass(TImagingTarga);
  Classes.UnRegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  TPicture.UnregisterGraphicClass(TImagingDDS);
  Classes.UnRegisterClass(TImagingDDS);
{$ENDIF}
  TPicture.UnregisterGraphicClass(TImagingGraphic);
  Classes.UnRegisterClass(TImagingGraphic);
end;

function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
begin
  case Format of
{$IFDEF COMPONENT_SET_VCL}
    ifIndex8: Result := pf8bit;
    ifR5G6B5: Result := pf16bit;
    ifR8G8B8: Result := pf24bit;
{$ENDIF}
    ifA8R8G8B8,
    ifX8R8G8B8: Result := pf32bit;
  else
    Result := pfCustom;
  end;
end;

function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;
begin
  case Format of
    pf8bit: Result := ifIndex8;
    pf15bit: Result := ifA1R5G5B5;
    pf16bit: Result := ifR5G6B5;
    pf24bit: Result := ifR8G8B8;
    pf32bit: Result := ifA8R8G8B8;
  else
    Result := ifUnknown;
  end;
end;

procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
var
  PF: TPixelFormat;
  Info: TImageFormatInfo;
  WorkData: TImageData;
{$IFDEF COMPONENT_SET_VCL}
  I, LineBytes: LongInt;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
{$ENDIF}
begin
  PF := DataFormatToPixelFormat(Data.Format);
  GetImageFormatInfo(Data.Format, Info);

  if (PF = pf8bit) and PaletteHasAlpha(Data.Palette, Info.PaletteEntries) then
  begin
    // Some indexed images may have valid alpha data, don't lose it!
    // (e.g. transparent 8bit PNG or GIF images)
    PF := pfCustom;
  end;

  if PF = pfCustom then
  begin
    // Convert from formats not supported by Graphics unit
    Imaging.InitImage(WorkData);
    Imaging.CloneImage(Data, WorkData);
    if Info.IsFloatingPoint or Info.HasAlphaChannel or Info.IsSpecial then
      Imaging.ConvertImage(WorkData, ifA8R8G8B8)
    else
    begin
{$IFDEF COMPONENT_SET_VCL}
      if Info.IsIndexed or Info.HasGrayChannel then
        Imaging.ConvertImage(WorkData, ifIndex8)
      else if Info.UsePixelFormat then
        Imaging.ConvertImage(WorkData, ifR5G6B5)
      else
        Imaging.ConvertImage(WorkData, ifR8G8B8);
{$ELSE}
        Imaging.ConvertImage(WorkData, ifA8R8G8B8);
{$ENDIF}
    end;

    PF := DataFormatToPixelFormat(WorkData.Format);
    GetImageFormatInfo(WorkData.Format, Info);
  end
  else
    WorkData := Data;
    
  if PF = pfCustom then
    RaiseImaging(SBadFormatDataToBitmap, [ImageToStr(WorkData)]);

{$IFDEF COMPONENT_SET_VCL}
  Bitmap.Width := WorkData.Width;
  Bitmap.Height := WorkData.Height;
  Bitmap.PixelFormat := PF;

  if (PF = pf8bit) and (WorkData.Palette <> nil) then
  begin
    // Copy palette, this must be done before copying bits
    FillChar(LogPalette, SizeOf(LogPalette), 0);
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := Info.PaletteEntries;
    for I := 0 to Info.PaletteEntries - 1 do
    with LogPalette do
    begin
      palPalEntry[I].peRed := WorkData.Palette[I].R;
      palPalEntry[I].peGreen := WorkData.Palette[I].G;
      palPalEntry[I].peBlue := WorkData.Palette[I].B;
    end;
    Bitmap.Palette := CreatePalette(PLogPalette(@LogPalette)^);
  end;

  // Copy scanlines
    LineBytes := WorkData.Width * Info.BytesPerPixel;
  for I := 0 to WorkData.Height - 1 do
    Move(PByteArray(WorkData.Bits)[I * LineBytes], Bitmap.Scanline[I]^, LineBytes);

  // Delphi 2009 and newer support alpha transparency for TBitmap
{$IF Defined(DELPHI) and (CompilerVersion >= 20.0)}
  if Bitmap.PixelFormat = pf32bit then
    Bitmap.AlphaFormat := afDefined;
{$IFEND}
{$ENDIF}

{$IFDEF COMPONENT_SET_LCL}
  // Create 32bit raw image from image data
  FillChar(RawImage, SizeOf(RawImage), 0);
  with RawImage.Description do
  begin
    Width := WorkData.Width;
    Height := WorkData.Height;
    BitsPerPixel := 32;
    Format := ricfRGBA;
    LineEnd := rileDWordBoundary;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    AlphaPrec := 8;
    RedPrec := 8;
    GreenPrec := 8;
    BluePrec := 8;
    AlphaShift := 24;
    RedShift := 16;
    GreenShift := 8;
    BlueShift := 0;
    Depth := 32; // Must be 32 for alpha blending (and for working in MacOSX Carbon)
  end;
  RawImage.Data := WorkData.Bits;
  RawImage.DataSize := WorkData.Size;

  // Create bitmap from raw image
  if RawImage_CreateBitmaps(RawImage, ImgHandle, ImgMaskHandle) then
  begin
    Bitmap.Handle := ImgHandle;
    Bitmap.MaskHandle := ImgMaskHandle;
  end;
{$ENDIF}
  if WorkData.Bits <> Data.Bits then
    Imaging.FreeImage(WorkData);
end;

procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
var
  I, LineBytes: LongInt;
  Format: TImageFormat;
  Info: TImageFormatInfo;
{$IFDEF COMPONENT_SET_VCL}
  Colors: Word;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  LineLazBytes: LongInt;
{$ENDIF}
begin
  Format := ifUnknown;
{$IFDEF COMPONENT_SET_LCL}
  // In the current Lazarus 0.9.10 Bitmap.PixelFormat property is useless.
  // We cannot change bitmap's format by changing it (it will just release
  // old image but not convert it to new format) nor we can determine bitmaps's
  // current format (it is usually set to pfDevice). So bitmap's format is obtained
  // trough RawImage api and cannot be changed to mirror some Imaging format
  // (so formats with no corresponding Imaging format cannot be saved now).

  if RawImage_DescriptionFromBitmap(Bitmap.Handle, RawImage.Description) then
    case RawImage.Description.BitsPerPixel of
      8: Format := ifIndex8;
      16:
        if RawImage.Description.Depth = 15 then
          Format := ifA1R5G5B5
        else
          Format := ifR5G6B5;
      24: Format := ifR8G8B8;
      32: Format := ifA8R8G8B8;
      48: Format := ifR16G16B16;
      64: Format := ifA16R16G16B16;
    end;
{$ELSE}
  Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  if Format = ifUnknown then
  begin
    // Convert from formats not supported by Imaging (1/4 bit)
    if Bitmap.PixelFormat < pf8bit then
       Bitmap.PixelFormat := pf8bit
    else
      Bitmap.PixelFormat := pf32bit;
    Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  end;
{$ENDIF}

  if Format = ifUnknown then
    RaiseImaging(SBadFormatBitmapToData, []);

  Imaging.NewImage(Bitmap.Width, Bitmap.Height, Format, Data);
  GetImageFormatInfo(Data.Format, Info);
  LineBytes := Data.Width * Info.BytesPerPixel;

{$IFDEF COMPONENT_SET_VCL}
  if (Format = ifIndex8) and (GetObject(Bitmap.Palette, SizeOf(Colors),
    @Colors) <> 0) then
  begin
    // Copy palette
    GetPaletteEntries(Bitmap.Palette, 0, Colors, LogPalette.palPalEntry);
    if Colors > Info.PaletteEntries  then
      Colors := Info.PaletteEntries;
    for I := 0 to Colors - 1 do
    with LogPalette do
    begin
      Data.Palette[I].A := $FF;
      Data.Palette[I].R := palPalEntry[I].peRed;
      Data.Palette[I].G := palPalEntry[I].peGreen;
      Data.Palette[I].B := palPalEntry[I].peBlue;
    end;
  end;
  // Copy scanlines
  for I := 0 to Data.Height - 1 do
    Move(Bitmap.ScanLine[I]^, PByteArray(Data.Bits)[I * LineBytes], LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // Get raw image from bitmap (mask handle must be 0 or expect violations)
  if RawImage_FromBitmap(RawImage, Bitmap.Handle, 0, nil) then
  begin
    LineLazBytes := GetBytesPerLine(Data.Width, RawImage.Description.BitsPerPixel,
      RawImage.Description.LineEnd);
    // Copy scanlines
    for I := 0 to Data.Height - 1 do
    begin
      Move(PByteArray(RawImage.Data)[I * LineLazBytes],
        PByteArray(Data.Bits)[I * LineBytes], LineBytes);
    end;
    // May need to swap RB order, depends on widget set
    if RawImage.Description.BlueShift > RawImage.Description.RedShift then
      SwapChannels(Data, ChannelRed, ChannelBlue);

    RawImage.FreeData;
  end;
{$ENDIF}
end;

procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
begin
  ConvertDataToBitmap(Image.ImageDataPointer^, Bitmap);
end;

procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);
begin
  ConvertBitmapToData(Bitmap, Image.ImageDataPointer^);
end;

{$IFDEF MSWINDOWS}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
var
  OldMode: Integer;
  BitmapInfo: Windows.TBitmapInfo;
  Bmp: TBitmap;
begin
  if TestImage(ImageData) then
  begin
    Assert(ImageData.Format in [ifA8R8G8B8, ifX8R8G8B8], SBadFormatDisplay);
    OldMode := Windows.SetStretchBltMode(DC, COLORONCOLOR);

    FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := ImageData.Width;
      biHeight := -ImageData.Height;
      biSizeImage := ImageData.Size;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
    end;

    try
       with SrcRect, ImageData do
        if Windows.StretchDIBits(DC, DstRect.Left, DstRect.Top,
          DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Left,
          Top, Right - Left, Bottom - Top, Bits, BitmapInfo, DIB_RGB_COLORS, SRCCOPY) <> Height then
        begin
          // StretchDIBits may fail on some occasions (error 487, http://support.microsoft.com/kb/269585).
          // This fallback is slow but works every time. Thanks to Sergey Galezdinov for the fix.
          Bmp := TBitmap.Create;
          try
            ConvertDataToBitmap(ImageData, Bmp);
            StretchBlt(DC, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
              Bmp.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
          finally
            Bmp.Free;
          end;
        end;
    finally
      Windows.SetStretchBltMode(DC, OldMode);
    end;
  end;
end;
{$ENDIF}

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData);
begin
  DisplayImageData(DstCanvas, DstRect, ImageData, Rect(0, 0, ImageData.Width, ImageData.Height));
end;

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$IF Defined(DCC) or Defined(LCLWIN32)} // Delphi or LCL Win32
begin
  DisplayImageDataOnDC(DstCanvas.Handle, DstRect, ImageData, SrcRect);
end;
{$ELSEIF Defined(LCLGTK2)}
  procedure GDKDrawBitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
    SrcWidth, SrcHeight: Integer; ImageData: TImageData);
  var
    P: TPoint;
  begin
    P := TGtkDeviceContext(Dest).Offset;
    Inc(DstX, P.X);
    Inc(DstY, P.Y);

    if ImageData.Format = ifR8G8B8 then
    begin
      gdk_draw_rgb_image(TGtkDeviceContext(Dest).Drawable, TGtkDeviceContext(Dest).GC,
        DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
        @PUInt32Array(ImageData.Bits)[SrcY * ImageData.Width + SrcX], ImageData.Width * 3);
    end
    else
    begin
      gdk_draw_rgb_32_image(TGtkDeviceContext(Dest).Drawable, TGtkDeviceContext(Dest).GC,
        DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
        @PUInt32Array(ImageData.Bits)[SrcY * ImageData.Width + SrcX], ImageData.Width * 4);
    end;
  end;

var
  DisplayImage: TImageData;
  NewWidth, NewHeight: Integer;
  SrcBounds, DstBounds, DstClip: TRect;
begin
  if TestImage(ImageData) then
  begin
    if not (ImageData.Format in [ifR8G8B8, ifA8R8G8B8, ifX8R8G8B8]) then
      raise EImagingError.Create(SBadFormatDisplay);

    InitImage(DisplayImage);
    SrcBounds := RectToBounds(SrcRect);
    DstBounds := RectToBounds(DstRect);
    WidgetSet.GetClipBox(DstCanvas.Handle, @DstClip);

    ClipStretchBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
      DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, ImageData.Width,
      ImageData.Height, DstClip);

    NewWidth := DstBounds.Right;
    NewHeight := DstBounds.Bottom;

    if (NewWidth > 0) and (NewHeight > 0) then
    begin
      if (SrcBounds.Right = NewWidth) and (SrcBounds.Bottom = NewHeight) then
      try
        CloneImage(ImageData, DisplayImage);
        // Swap R-B channels for GTK display compatibility!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top,
          SrcBounds.Left, SrcBounds.Top, NewWidth, NewHeight, DisplayImage);
      finally
        FreeImage(DisplayImage);
      end
      else
      try
        // Create new image with desired dimensions
        NewImage(NewWidth, NewHeight, ImageData.Format, DisplayImage);
        // Stretch pixels from old image to new one  TResizeFilter = (rfNearest, rfBilinear, rfBicubic);
        StretchRect(ImageData, SrcBounds.Left, SrcBounds.Top, SrcBounds.Right,
          SrcBounds.Bottom, DisplayImage, 0, 0, NewWidth, NewHeight, rfNearest);
        // Swap R-B channels for GTK display compatibility!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top, 0, 0,
          NewWidth, NewHeight, DisplayImage);
       finally
        FreeImage(DisplayImage);
      end
    end;
  end;
end;
{$ELSE}
begin
  raise EImagingError.Create(SUnsupportedLCLWidgetSet);
end;
{$IFEND}

procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, BoundsToRect(DstX, DstY, Image.Width, Image.Height),
    Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, SrcRect);
end;


{ TImagingGraphic class implementation }

constructor TImagingGraphic.Create;
begin
  inherited Create;
  PixelFormat := pf24Bit;
end;

procedure TImagingGraphic.ReadData(Stream: TStream);
begin
  // Here we need to skip ReadData+WriteData of TBitmap (and LCL TRasterBitmap)
  // and go to the basics in TGraphic's ReadData+WriteData with just LoadFromStream
  // and SaveToStream.
  // Some VCL/LCL TGraphic classes also store size of the written data
  // before the stream contents. However, the stream passed here
  // from TReader.DefineBinaryProperty is already
  // a memory stream capped to the size of binary property data.
  // Picture.Data = <vaBinary><Size(TWriter)><TGraphicClassName(TPicture)><ImageBits(TImagingGraphicForSave)>
  LoadFromStream(Stream);
end;

procedure TImagingGraphic.WriteData(Stream: TStream);
begin
  // This should never occur in the wild since IDE would create one of the descendants
  // of TImagingGraphicForSave for specific format+extension when e.g. setting TPicture of TImage.
  raise ENotImplemented.CreateFmt('Not implemented: Use TImagingGraphicForSave class for writing with TFiler.', []);
end;

procedure TImagingGraphic.LoadFromStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  Image := TSingleImage.Create;
  try
    Image.LoadFromStream(Stream);
    Assign(Image);
  finally
    Image.Free;
  end;
end;

procedure TImagingGraphic.AssignTo(Dest: TPersistent);
var
  Arr: TDynImageDataArray;
begin
  if Dest is TSingleImage then
  begin
    AssignToImage(TSingleImage(Dest))
  end
  else if Dest is TMultiImage then
  begin
    SetLength(Arr, 1);
    AssignToImageData(Arr[0]);
    TMultiImage(Dest).CreateFromArray(Arr);
    Imaging.FreeImagesInArray(Arr);
  end
  else
    inherited AssignTo(Dest);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingGraphic.GetResourceType: TResourceType;
begin
  Result := RT_RCDATA;
end;

class function TImagingGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
begin
  Result := DetermineStreamFormat(Stream) <> '';
end;
{$ENDIF}

procedure TImagingGraphic.Assign(Source: TPersistent);
begin
  if Source is TBaseImage then
    AssignFromImage(TBaseImage(Source))
  else
    inherited Assign(Source);
end;

procedure TImagingGraphic.AssignFromImage(Image: TBaseImage);
begin
  if (Image <> nil) and Image.Valid then
    AssignFromImageData(Image.ImageDataPointer^);
end;

procedure TImagingGraphic.AssignToImage(Image: TBaseImage);
begin
  if (Image <> nil) and (Image.ImageDataPointer <> nil) then
    AssignToImageData(Image.ImageDataPointer^);
end;

procedure TImagingGraphic.AssignFromImageData(const ImageData: TImageData);
begin
  if Imaging.TestImage(ImageData) then
    ConvertDataToBitmap(ImageData, Self);
end;

procedure TImagingGraphic.AssignToImageData(var ImageData: TImageData);
begin
  Imaging.FreeImage(ImageData);
  ConvertBitmapToData(Self, ImageData);
end;


{ TImagingGraphicForSave class implementation }

constructor TImagingGraphicForSave.Create;
begin
  inherited Create;
  FDefaultFileExt := GetFileFormat.Extensions[0];
  FSavingFormat := ifUnknown;
  GetFileFormat.CheckOptionsValidity;
end;

procedure TImagingGraphicForSave.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TImagingGraphicForSave.SaveToStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  if FDefaultFileExt <> '' then
  begin
    Image := TSingleImage.Create;
    try
      Image.Assign(Self);
      if FSavingFormat <> ifUnknown then
        Image.Format := FSavingFormat;
      Image.SaveToStream(FDefaultFileExt, Stream);
    finally
      Image.Free;
    end;
  end;
end;

{$IFDEF COMPONENT_SET_LCL}
class function TImagingGraphicForSave.GetFileExtensions: string;
begin
  Result := StringReplace(GetFileFormat.Extensions.CommaText, ',', ';', [rfReplaceAll]);
end;

function TImagingGraphicForSave.GetMimeType: string;
begin
  Result := 'image/' + FDefaultFileExt;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_BITMAP}

{ TImagingBitmap class implementation }

constructor TImagingBitmap.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TBitmapFileFormat).UseRLE;
end;

class function TImagingBitmap.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TBitmapFileFormat);
end;

procedure TImagingBitmap.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingBitmapRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}

{ TImagingJpeg class implementation }

constructor TImagingJpeg.Create;
begin
  inherited Create;
  FQuality := (GetFileFormat as TJpegFileFormat).Quality;
  FProgressive := (GetFileFormat as TJpegFileFormat).Progressive;
end;

class function TImagingJpeg.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJpegFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingJpeg.GetMimeType: string;
begin
  Result := 'image/jpeg';
end;
{$ENDIF}

procedure TImagingJpeg.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJpegQuality, FQuality);
  Imaging.SetOption(ImagingJpegProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;

{$ENDIF}

{$IFNDEF DONT_LINK_PNG}

{ TImagingPNG class implementation }

constructor TImagingPNG.Create;
begin
  inherited Create;
  FPreFilter := (GetFileFormat as TPNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TPNGFileFormat).CompressLevel;
end;

class function TImagingPNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TPNGFileFormat);
end;

procedure TImagingPNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingPNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingPNGCompressLevel, FCompressLevel);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_GIF}

{ TImagingGIF class implementation}

class function TImagingGIF.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TGIFFileFormat);
end;

{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}

{ TImagingTarga class implementation }

constructor TImagingTarga.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TTargaFileFormat).UseRLE;
end;

class function TImagingTarga.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TTargaFileFormat);
end;

procedure TImagingTarga.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingTargaRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}

{ TImagingDDS class implementation }

constructor TImagingDDS.Create;
begin
  inherited Create;
  FCompression := dcNone;
end;

class function TImagingDDS.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TDDSFileFormat);
end;

procedure TImagingDDS.SaveToStream(Stream: TStream);
begin
  case FCompression of
    dcNone: FSavingFormat := ifUnknown;
    dcDXT1: FSavingFormat := ifDXT1;
    dcDXT3: FSavingFormat := ifDXT3;
    dcDXT5: FSavingFormat := ifDXT5;
  end;
  Imaging.PushOptions;
  Imaging.SetOption(ImagingDDSSaveCubeMap, Ord(False));
  Imaging.SetOption(ImagingDDSSaveVolume, Ord(False));
  Imaging.SetOption(ImagingDDSSaveMipMapCount, 1);
  Imaging.SetOption(ImagingDDSSaveDepth, 1);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}

{ TImagingMNG class implementation }

constructor TImagingMNG.Create;
begin
  inherited Create;
  FLossyCompression := (GetFileFormat as TMNGFileFormat).LossyCompression;
  FLossyAlpha := (GetFileFormat as TMNGFileFormat).LossyAlpha;
  FPreFilter := (GetFileFormat as TMNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TMNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TMNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TMNGFileFormat).Progressive;
end;

class function TImagingMNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TMNGFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingMNG.GetMimeType: string;
begin
  Result := 'video/mng';
end;
{$ENDIF}

procedure TImagingMNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingMNGLossyCompression, Ord(FLossyCompression));
  Imaging.SetOption(ImagingMNGLossyAlpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingMNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingMNGCompressLevel, FCompressLevel);
  Imaging.SetOption(ImagingMNGQuality, FQuality);
  Imaging.SetOption(ImagingMNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}

{ TImagingJNG class implementation }

constructor TImagingJNG.Create;
begin
  inherited Create;
  FLossyAlpha := (GetFileFormat as TJNGFileFormat).LossyAlpha;
  FAlphaPreFilter := (GetFileFormat as TJNGFileFormat).PreFilter;
  FAlphaCompressLevel := (GetFileFormat as TJNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TJNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TJNGFileFormat).Progressive;
end;

class function TImagingJNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJNGFileFormat);
end;

procedure TImagingJNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJNGLossyALpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingJNGAlphaPreFilter, FAlphaPreFilter);
  Imaging.SetOption(ImagingJNGAlphaCompressLevel, FAlphaCompressLevel);
  Imaging.SetOption(ImagingJNGQuality, FQuality);
  Imaging.SetOption(ImagingJNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

initialization
  RegisteredFormats := TList.Create;
  RegisterTypes;
finalization
  UnRegisterTypes;
  RegisteredFormats.Free;

{$IFEND} // {$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Fixed bug in ConvertBitmapToData causing images from GTK2 bitmaps
      to have swapped RB channels.
    - LCL: Removed GTK1 support (deprecated).

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Transparency of 8bit images (like loaded from 8bit PNG or GIF) is
      kept intact during conversion to TBitmap in ConvertDataToBitmap
      (32bit bitmap is created).

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Setting AlphaFormat property of TBitmap in ConvertDataToBitmap
      when using Delphi 2009+.
    - Fixed garbled LCL TBitmaps created by ConvertDataToBitmap
      in Mac OS X (Carbon).

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added some more IFDEFs for Lazarus widget sets.
    - Removed CLX code.
    - GTK version of Unix DisplayImageData only used with LCL GTK so the
      the rest of the unit can be used with Qt or other LCL interfaces. 
    - Fallback mechanism for DisplayImageDataOnDC, it may fail on occasions.
    - Changed file format conditional compilation to reflect changes
      in LINK symbols.
    - Lazarus 0.9.26 compatibility changes.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Fixed wrong IFDEF causing that Imaging wouldn't compile in Lazarus
      with GTK2 target.
    - Added comments with code for Lazarus rev. 11861+ regarding
      RawImage interface. Replace current code with that in comments
      if you use Lazarus from SVN. New RawImage interface will be used by
      default after next Lazarus release. 

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added TImagingGIF. 

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Uses only high level interface now (except for saving options).
    - Slightly changed class hierarchy. TImagingGraphic is now only for loading
      and base class for savers is new TImagingGraphicForSave. Also
      TImagingGraphic is now registered with all supported file formats
      by TPicture's format support.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added DisplayImage procedures (thanks to Paul Michell, modified)
    - removed RegisterTypes and UnRegisterTypes from interface section,
      they are called automatically
    - added procedures: ConvertImageToBitmap and ConvertBitmapToImage

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - LCL data to bitmap conversion didn't work in Linux, fixed
    - added MNG file format
    - added JNG file format

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - made it LCL compatible
    - made it CLX compatible
    - added all initial stuff
}

end.

