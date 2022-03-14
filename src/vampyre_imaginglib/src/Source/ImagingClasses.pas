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

{ This unit contains class based wrapper to Imaging library.}
unit ImagingClasses;

{$I ImagingOptions.inc}

interface

uses
  Types, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Base abstract high level class wrapper to low level Imaging structures and
    functions.}
  TBaseImage = class(TPersistent)
  private
    function GetEmpty: Boolean;
  protected
    FPData: PImageData;
    FOnDataSizeChanged: TNotifyEvent;
    FOnPixelsChanged: TNotifyEvent;
    function GetFormat: TImageFormat; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetHeight: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetSize: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetWidth: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBits: Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetPalette: PPalette32; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetPaletteEntries: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetScanline(Index: Integer): Pointer;
    function GetPixelPointer(X, Y: Integer): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetScanlineSize: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetFormatInfo: TImageFormatInfo; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetValid: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBoundsRect: TRect;
    procedure SetFormat(const Value: TImageFormat); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetHeight(const Value: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetWidth(const Value: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPointer; virtual; abstract;
    procedure DoDataSizeChanged; virtual;
    procedure DoPixelsChanged; virtual;
  public
    constructor Create; virtual;
    constructor CreateFromImage(AImage: TBaseImage);
    destructor Destroy; override;
    { Returns info about current image.}
    function ToString: string; {$IF (Defined(DCC) and (CompilerVersion >= 20.0)) or Defined(FPC)}override;{$IFEND}

    { Creates a new image data with the given size and format. Old image
      data is lost. Works only for the current image of TMultiImage.}
    procedure RecreateImageData(AWidth, AHeight: Integer; AFormat: TImageFormat);
    { Maps underlying image data to given TImageData record. Both TBaseImage and
      TImageData now share some image memory (bits). So don't call FreeImage
      on TImageData afterwards since this TBaseImage would get really broken.}
    procedure MapImageData(const ImageData: TImageData);
    { Deletes current image.}
    procedure Clear;

    { Resizes current image with optional resampling.}
    procedure Resize(NewWidth, NewHeight: Integer; Filter: TResizeFilter);
    { Resizes current image proportionally to fit the given width and height. }
    procedure ResizeToFit(FitWidth, FitHeight: Integer; Filter: TResizeFilter; DstImage: TBaseImage);
    { Flips current image. Reverses the image along its horizontal axis the top
      becomes the bottom and vice versa.}
    procedure Flip;
    { Mirrors current image. Reverses the image along its vertical axis the left
      side becomes the right and vice versa.}
    procedure Mirror;
    { Rotates image by Angle degrees counterclockwise.}
    procedure Rotate(Angle: Single);
    { Copies rectangular part of SrcImage to DstImage. No blending is performed -
      alpha is simply copied to destination image. Operates also with
      negative X and Y coordinates.
      Note that copying is fastest for images in the same data format
      (and slowest for images in special formats).}
    procedure CopyTo(SrcX, SrcY, Width, Height: Integer; DstImage: TBaseImage; DstX, DstY: Integer); overload;
    { Copies whole image to DstImage. No blending is performed -
      alpha is simply copied to destination image. Operates also with
      negative X and Y coordinates.
      Note that copying is fastest for images in the same data format
      (and slowest for images in special formats).}
    procedure CopyTo(DstImage: TBaseImage; DstX, DstY: Integer); overload;
    { Stretches the contents of the source rectangle to the destination rectangle
      with optional resampling. No blending is performed - alpha is
      simply copied/resampled to destination image. Note that stretching is
      fastest for images in the same data format (and slowest for
      images in special formats).}
    procedure StretchTo(SrcX, SrcY, SrcWidth, SrcHeight: Integer; DstImage: TBaseImage; DstX, DstY, DstWidth, DstHeight: Integer; Filter: TResizeFilter);
    { Replaces pixels with OldPixel in the given rectangle by NewPixel.
      OldPixel and NewPixel should point to the pixels in the same format
      as the given image is in.}
    procedure ReplaceColor(X, Y, Width, Height: Integer; OldColor, NewColor: Pointer);
    { Swaps SrcChannel and DstChannel color or alpha channels of image.
      Use ChannelRed, ChannelBlue, ChannelGreen, ChannelAlpha constants to
      identify channels.}
    procedure SwapChannels(SrcChannel, DstChannel: Integer);

    { Loads current image data from file.}
    procedure LoadFromFile(const FileName: string); virtual;
    { Loads current image data from stream.}
    procedure LoadFromStream(Stream: TStream); virtual;

    { Saves current image data to file.}
    function SaveToFile(const FileName: string): Boolean;
    { Saves current image data to stream. Ext identifies desired image file
      format (jpg, png, dds, ...).}
    function SaveToStream(const Ext: string; Stream: TStream): Boolean;

    { Width of current image in pixels.}
    property Width: Integer read GetWidth write SetWidth;
    { Height of current image in pixels.}
    property Height: Integer read GetHeight write SetHeight;
    { Image data format of current image.}
    property Format: TImageFormat read GetFormat write SetFormat;
    { Size in bytes of current image's data.}
    property Size: Integer read GetSize;
    { Pointer to memory containing image bits.}
    property Bits: Pointer read GetBits;
    { Pointer to palette for indexed format images. It is nil for others.
      Max palette entry is at index [PaletteEntries - 1].}
    property Palette: PPalette32 read GetPalette;
    { Number of entries in image's palette}
    property PaletteEntries: Integer read GetPaletteEntries;
    { Provides indexed access to each line of pixels. Does not work with special
      format images (like DXT).}
    property Scanline[Index: Integer]: Pointer read GetScanline;
    { Returns pointer to image pixel at [X, Y] coordinates.}
    property PixelPointer[X, Y: Integer]: Pointer read GetPixelPointer;
    { Size/length of one image scanline in bytes.}
    property ScanlineSize: Integer read GetScanlineSize;
    { Extended image format information.}
    property FormatInfo: TImageFormatInfo read GetFormatInfo;
    { This gives complete access to underlying TImageData record.
      It can be used in functions that take TImageData as parameter
      (for example: ReduceColors(SingleImageInstance.ImageData^, 64)).}
    property ImageDataPointer: PImageData read FPData;
    { Indicates whether the current image is valid (proper format,
      allowed dimensions, right size, ...).}
    property Valid: Boolean read GetValid;
    { Indicates whether image contains any data (size in bytes > 0).}
    property Empty: Boolean read GetEmpty;
    { Specifies the bounding rectangle of the image.}
    property BoundsRect: TRect read GetBoundsRect;
    { This event occurs when the image data size has just changed. That means
      image width, height, or format has been changed.}
    property OnDataSizeChanged: TNotifyEvent read FOnDataSizeChanged write FOnDataSizeChanged;
    { This event occurs when some pixels of the image have just changed.}
    property OnPixelsChanged: TNotifyEvent read FOnPixelsChanged write FOnPixelsChanged;
  end;

  { Extension of TBaseImage which uses single TImageData record to
    store image. All methods inherited from TBaseImage work with this record.}
  TSingleImage = class(TBaseImage)
  protected
    FImageData: TImageData;
    procedure SetPointer; override;
  public
    constructor Create; override;
    constructor CreateFromParams(AWidth, AHeight: Integer; AFormat: TImageFormat = ifDefault);
    constructor CreateFromData(const AData: TImageData);
    constructor CreateFromFile(const FileName: string);
    constructor CreateFromStream(Stream: TStream);
    destructor Destroy; override;
    { Assigns single image from another single image or multi image.}
    procedure Assign(Source: TPersistent); override;
    { Assigns single image from image data record.}
    procedure AssignFromImageData(const AImageData: TImageData);
  end;

  { Extension of TBaseImage which uses array of TImageData records to
    store multiple images. Images are independent on each other and they don't
    share any common characteristic. Each can have different size, format, and
    palette. All methods inherited from TBaseImage work only with
    active image (it could represent mipmap level, animation frame, or whatever).
    Methods whose names contain word 'Multi' work with all images in array
    (as well as other methods with obvious names).}
  TMultiImage = class(TBaseImage)
  protected
    FDataArray: TDynImageDataArray;
    FActiveImage: Integer;
    procedure SetActiveImage(Value: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetImageCount: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetImageCount(Value: Integer);
    function GetAllImagesValid: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetImage(Index: Integer): TImageData; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetImage(Index: Integer; Value: TImageData); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPointer; override;
    function PrepareInsert(Index, InsertCount: Integer): Boolean;
    procedure DoInsertImages(Index: Integer; const Images: TDynImageDataArray);
    procedure DoInsertNew(Index: Integer; AWidth, AHeight: Integer; AFormat: TImageFormat);
  public
    constructor Create; override;
    constructor CreateFromParams(AWidth, AHeight: Integer; AFormat: TImageFormat; ImageCount: Integer);
    constructor CreateFromArray(const ADataArray: TDynImageDataArray);
    constructor CreateFromFile(const FileName: string);
    constructor CreateFromStream(Stream: TStream);
    destructor Destroy; override;
    { Assigns multi image from another multi image or single image.}
    procedure Assign(Source: TPersistent); override;
    { Assigns multi image from array of image data records.}
    procedure AssignFromArray(const ADataArray: TDynImageDataArray);

    { Adds new image at the end of the image array. Returns index of the added image.}
    function AddImage(AWidth, AHeight: Integer; AFormat: TImageFormat = ifDefault): Integer; overload;
    { Adds existing image at the end of the image array. Returns index of the added image.}
    function AddImage(const Image: TImageData): Integer; overload;
    { Adds existing image (or active image of a TMultiImage)
      at the end of the image array. Returns index of the added image.}
    function AddImage(Image: TBaseImage): Integer; overload;
    { Adds existing image array (all images of a multi image)
      at the end of the image array.}
    procedure AddImages(const Images: TDynImageDataArray); overload;
    { Adds existing MultiImage images at the end of the image array.}
    procedure AddImages(Images: TMultiImage); overload;

    { Inserts new image image at the given position in the image array. }
    procedure InsertImage(Index, AWidth, AHeight: Integer; AFormat: TImageFormat = ifDefault); overload;
    { Inserts existing image at the given position in the image array. }
    procedure InsertImage(Index: Integer; const Image: TImageData); overload;
    { Inserts existing image (Active image of a TMultiImage)
      at the given position in the image array. }
    procedure InsertImage(Index: Integer; Image: TBaseImage); overload;
    { Inserts existing image at the given position in the image array. }
    procedure InsertImages(Index: Integer; const Images: TDynImageDataArray); overload;
    { Inserts existing images (all images of a TMultiImage) at
      the given position in the image array. }
    procedure InsertImages(Index: Integer; Images: TMultiImage); overload;

    { Exchanges two images at the given positions in the image array. }
    procedure ExchangeImages(Index1, Index2: Integer);
    { Deletes image at the given position in the image array.}
    procedure DeleteImage(Index: Integer);
    { Rearranges images so that the first image will become last and vice versa.}
    procedure ReverseImages;
    { Deletes all images.}
    procedure ClearAll;

    { Converts all images to another image data format.}
    procedure ConvertImages(Format: TImageFormat);
    { Resizes all images.}
    procedure ResizeImages(NewWidth, NewHeight: Integer; Filter: TResizeFilter);

    { Overloaded loading method that will add new image to multi-image if
      image array is empty before loading. If it's not empty the active image is replaced.}
    procedure LoadFromFile(const FileName: string); override;
    { Overloaded loading method that will add new image to multi-image if
      image array is empty before loading. If it's not empty the active image is replaced.}
    procedure LoadFromStream(Stream: TStream); override;

    { Loads whole multi image from file.}
    procedure LoadMultiFromFile(const FileName: string);
    { Loads whole multi image from stream.}
    procedure LoadMultiFromStream(Stream: TStream);
    { Saves whole multi image to file.}
    function SaveMultiToFile(const FileName: string): Boolean;
    { Saves whole multi image to stream. Ext identifies desired
      image file format (jpg, png, dds, ...).}
    function SaveMultiToStream(const Ext: string; Stream: TStream): Boolean;

    { Indicates active image of this multi image. All methods inherited
      from TBaseImage operate on this image only.}
    property ActiveImage: Integer read FActiveImage write SetActiveImage;
    { Number of images of this multi image.}
    property ImageCount: Integer read GetImageCount write SetImageCount;
    { This value is True if all images of this TMultiImage are valid.}
    property AllImagesValid: Boolean read GetAllImagesValid;
    { This gives complete access to underlying TDynImageDataArray.
      It can be used in functions that take TDynImageDataArray
      as parameter.}
    property DataArray: TDynImageDataArray read FDataArray;
    { Array property for accessing individual images of TMultiImage. When you
      set image at given index the old image is freed and the source is cloned.}
    property Images[Index: Integer]: TImageData read GetImage write SetImage; default;
  end;

implementation

const
  DefaultWidth = 16;
  DefaultHeight = 16;

function GetArrayFromImageData(const ImageData: TImageData): TDynImageDataArray;
begin
  SetLength(Result, 1);
  Result[0] := ImageData;
end;

{ TBaseImage class implementation }

constructor TBaseImage.Create;
begin
  SetPointer;
end;

constructor TBaseImage.CreateFromImage(AImage: TBaseImage);
begin
  Create;
  Assign(AImage);
end;

destructor TBaseImage.Destroy;
begin
  inherited Destroy;
end;

function TBaseImage.GetWidth: Integer;
begin
  if Valid then
    Result := FPData.Width
  else
    Result := 0;
end;

function TBaseImage.GetHeight: Integer;
begin
  if Valid then
    Result := FPData.Height
  else
    Result := 0;
end;

function TBaseImage.GetFormat: TImageFormat;
begin
  if Valid then
    Result := FPData.Format
  else
    Result := ifUnknown;
end;

function TBaseImage.GetScanline(Index: Integer): Pointer;
var
  Info: TImageFormatInfo;
begin
  if Valid then
  begin
    Info := GetFormatInfo;
    if not Info.IsSpecial then
      Result := ImagingFormats.GetScanLine(FPData.Bits, Info, FPData.Width, Index)
    else
      Result := FPData.Bits;
  end
  else
    Result := nil;
end;

function TBaseImage.GetScanlineSize: Integer;
begin
  if Valid then
    Result := FormatInfo.GetPixelsSize(Format, Width, 1)
  else
    Result := 0;
end;

function TBaseImage.GetPixelPointer(X, Y: Integer): Pointer;
begin
  if Valid then
    Result := @PByteArray(FPData.Bits)[(Y * FPData.Width + X) * GetFormatInfo.BytesPerPixel]
  else
    Result := nil;
end;

function TBaseImage.GetSize: Integer;
begin
  if Valid then
    Result := FPData.Size
  else
    Result := 0;
end;

function TBaseImage.GetBits: Pointer;
begin
  if Valid then
    Result := FPData.Bits
  else
    Result := nil;
end;

function TBaseImage.GetPalette: PPalette32;
begin
  if Valid then
    Result := FPData.Palette
  else
    Result := nil;
end;

function TBaseImage.GetPaletteEntries: Integer;
begin
  Result := GetFormatInfo.PaletteEntries;
end;

function TBaseImage.GetFormatInfo: TImageFormatInfo;
begin
  if Valid then
    Imaging.GetImageFormatInfo(FPData.Format, Result)
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBaseImage.GetValid: Boolean;
begin
  Result := Assigned(FPData) and Imaging.TestImage(FPData^);
end;

function TBaseImage.GetBoundsRect: TRect;
begin
  Result := Rect(0, 0, GetWidth, GetHeight);
end;

function TBaseImage.GetEmpty: Boolean;
begin
  Result := FPData.Size = 0;
end;

procedure TBaseImage.SetWidth(const Value: Integer);
begin
  Resize(Value, GetHeight, rfNearest);
end;

procedure TBaseImage.SetHeight(const Value: Integer);
begin
  Resize(GetWidth, Value, rfNearest);
end;

procedure TBaseImage.SetFormat(const Value: TImageFormat);
begin
  if Valid and Imaging.ConvertImage(FPData^, Value) then
    DoDataSizeChanged;
end;

procedure TBaseImage.DoDataSizeChanged;
begin
  if Assigned(FOnDataSizeChanged) then
    FOnDataSizeChanged(Self);
  DoPixelsChanged;
end;

procedure TBaseImage.DoPixelsChanged;
begin
  if Assigned(FOnPixelsChanged) then
    FOnPixelsChanged(Self);
end;

procedure TBaseImage.RecreateImageData(AWidth, AHeight: Integer; AFormat: TImageFormat);
begin
  if Assigned(FPData) and Imaging.NewImage(AWidth, AHeight, AFormat, FPData^) then
    DoDataSizeChanged;
end;

procedure TBaseImage.MapImageData(const ImageData: TImageData);
begin
  Clear;
  FPData.Width := ImageData.Width;
  FPData.Height := ImageData.Height;
  FPData.Format := ImageData.Format;
  FPData.Size := ImageData.Size;
  FPData.Bits := ImageData.Bits;
  FPData.Palette := ImageData.Palette;
end;

procedure TBaseImage.Clear;
begin
  FreeImage(FPData^);
end;

procedure TBaseImage.Resize(NewWidth, NewHeight: Integer; Filter: TResizeFilter);
begin
  if Valid and Imaging.ResizeImage(FPData^, NewWidth, NewHeight, Filter) then
    DoDataSizeChanged;
end;

procedure TBaseImage.ResizeToFit(FitWidth, FitHeight: Integer;
  Filter: TResizeFilter; DstImage: TBaseImage);
begin
  if Valid and Assigned(DstImage) then
  begin
    Imaging.ResizeImageToFit(FPData^, FitWidth, FitHeight, Filter,
      DstImage.FPData^);
    DstImage.DoDataSizeChanged;
  end;
end;

procedure TBaseImage.Flip;
begin
  if Valid and Imaging.FlipImage(FPData^) then
    DoPixelsChanged;
end;

procedure TBaseImage.Mirror;
begin
  if Valid and Imaging.MirrorImage(FPData^) then
    DoPixelsChanged;
end;

procedure TBaseImage.Rotate(Angle: Single);
begin
  if Valid then
  begin
    Imaging.RotateImage(FPData^, Angle);
    DoPixelsChanged;
  end;
end;

procedure TBaseImage.CopyTo(SrcX, SrcY, Width, Height: Integer;
  DstImage: TBaseImage; DstX, DstY: Integer);
begin
  if Valid and Assigned(DstImage) and DstImage.Valid then
  begin
    Imaging.CopyRect(FPData^, SrcX, SrcY, Width, Height, DstImage.FPData^, DstX, DstY);
    DstImage.DoPixelsChanged;
  end;
end;

procedure TBaseImage.CopyTo(DstImage: TBaseImage; DstX, DstY: Integer);
begin
  if Valid and Assigned(DstImage) and DstImage.Valid then
  begin
    Imaging.CopyRect(FPData^, 0, 0, Width, Height, DstImage.FPData^, DstX, DstY);
    DstImage.DoPixelsChanged;
  end;
end;

procedure TBaseImage.StretchTo(SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  DstImage: TBaseImage; DstX, DstY, DstWidth, DstHeight: Integer; Filter: TResizeFilter);
begin
  if Valid and Assigned(DstImage) and DstImage.Valid then
  begin
    Imaging.StretchRect(FPData^, SrcX, SrcY, SrcWidth, SrcHeight,
      DstImage.FPData^, DstX, DstY, DstWidth, DstHeight, Filter);
    DstImage.DoPixelsChanged;
  end;
end;

procedure TBaseImage.ReplaceColor(X, Y, Width, Height: Integer; OldColor,
  NewColor: Pointer);
begin
  if Valid then
  begin
    Imaging.ReplaceColor(FPData^, X, Y, Width, Height, OldColor, NewColor);
    DoPixelsChanged;
  end;
end;

procedure TBaseImage.SwapChannels(SrcChannel, DstChannel: Integer);
begin
  if Valid then
  begin
    Imaging.SwapChannels(FPData^, SrcChannel, DstChannel);
    DoPixelsChanged;
  end;
end;

function TBaseImage.ToString: string;
begin
  Result := Iff(Valid, Imaging.ImageToStr(FPData^), 'empty image');
end;

procedure TBaseImage.LoadFromFile(const FileName: string);
begin
  if Assigned(FPData) and Imaging.LoadImageFromFile(FileName, FPData^) then
    DoDataSizeChanged;
end;

procedure TBaseImage.LoadFromStream(Stream: TStream);
begin
  if Assigned(FPData) and Imaging.LoadImageFromStream(Stream, FPData^) then
    DoDataSizeChanged;
end;

function TBaseImage.SaveToFile(const FileName: string): Boolean;
begin
  if Valid then
    Result := Imaging.SaveImageToFile(FileName, FPData^)
  else
    Result := False;
end;

function TBaseImage.SaveToStream(const Ext: string; Stream: TStream): Boolean;
begin
  if Valid then
    Result := Imaging.SaveImageToStream(Ext, Stream, FPData^)
  else
    Result := False;
end;


{ TSingleImage class implementation }

constructor TSingleImage.Create;
begin
  inherited Create;
  Clear;
end;

constructor TSingleImage.CreateFromParams(AWidth, AHeight: Integer; AFormat: TImageFormat);
begin
  inherited Create;
  RecreateImageData(AWidth, AHeight, AFormat);
end;

constructor TSingleImage.CreateFromData(const AData: TImageData);
begin
  inherited Create;
  AssignFromImageData(AData);
end;

constructor TSingleImage.CreateFromFile(const FileName: string);
begin
  inherited Create;
  LoadFromFile(FileName);
end;

constructor TSingleImage.CreateFromStream(Stream: TStream);
begin
  inherited Create;
  LoadFromStream(Stream);
end;

destructor TSingleImage.Destroy;
begin
  Imaging.FreeImage(FImageData);
  inherited Destroy;
end;

procedure TSingleImage.SetPointer;
begin
  FPData := @FImageData;
end;

procedure TSingleImage.Assign(Source: TPersistent);
begin
  if Source = nil then
  begin
    Clear;
  end
  else if Source is TSingleImage then
  begin
    AssignFromImageData(TSingleImage(Source).FImageData);
  end
  else if Source is TMultiImage then
  begin
    if TMultiImage(Source).Valid then
      AssignFromImageData(TMultiImage(Source).FPData^)
    else
      Clear;
  end
  else
    inherited Assign(Source);
end;

procedure TSingleImage.AssignFromImageData(const AImageData: TImageData);
begin
  if Imaging.TestImage(AImageData) then
  begin
    Imaging.CloneImage(AImageData, FImageData);
    DoDataSizeChanged;
  end
  else
    Clear;
end;

{ TMultiImage class implementation }

constructor TMultiImage.Create;
begin
  inherited Create;
end;

constructor TMultiImage.CreateFromParams(AWidth, AHeight: Integer;
  AFormat: TImageFormat; ImageCount: Integer);
var
  I: Integer;
begin
  Imaging.FreeImagesInArray(FDataArray);
  SetLength(FDataArray, ImageCount);
  for I := 0 to GetImageCount - 1 do
    Imaging.NewImage(AWidth, AHeight, AFormat, FDataArray[I]);
  if GetImageCount > 0 then
    SetActiveImage(0);
end;

constructor TMultiImage.CreateFromArray(const ADataArray: TDynImageDataArray);
begin
  AssignFromArray(ADataArray);
end;

constructor TMultiImage.CreateFromFile(const FileName: string);
begin
  LoadMultiFromFile(FileName);
end;

constructor TMultiImage.CreateFromStream(Stream: TStream);
begin
  LoadMultiFromStream(Stream);
end;

destructor TMultiImage.Destroy;
begin
  Imaging.FreeImagesInArray(FDataArray);
  inherited Destroy;
end;

procedure TMultiImage.SetActiveImage(Value: Integer);
begin
  FActiveImage := Value;
  SetPointer;
end;

function TMultiImage.GetImageCount: Integer;
begin
  Result := Length(FDataArray);
end;

procedure TMultiImage.SetImageCount(Value: Integer);
var
  I, OldCount: Integer;
begin
  if Value > GetImageCount then
  begin
    // Create new empty images if array will be enlarged
    OldCount := GetImageCount;
    SetLength(FDataArray, Value);
    for I := OldCount to Value - 1 do
      Imaging.NewImage(DefaultWidth, DefaultHeight, ifDefault, FDataArray[I]);
  end
  else
  begin
    // Free images that exceed desired count and shrink array
    for I := Value to GetImageCount - 1 do
      Imaging.FreeImage(FDataArray[I]);
    SetLength(FDataArray, Value);
  end;
  SetPointer;
end;

function TMultiImage.GetAllImagesValid: Boolean;
begin
  Result := (GetImageCount > 0) and TestImagesInArray(FDataArray);
end;

function TMultiImage.GetImage(Index: Integer): TImageData;
begin
  if (Index >= 0) and (Index < GetImageCount) then
    Result := FDataArray[Index];
end;

procedure TMultiImage.SetImage(Index: Integer; Value: TImageData);
begin
  if (Index >= 0) and (Index < GetImageCount) then
    Imaging.CloneImage(Value, FDataArray[Index]);
end;

procedure TMultiImage.SetPointer;
begin
  if GetImageCount > 0 then
  begin
    FActiveImage := ClampInt(FActiveImage, 0, GetImageCount - 1);
    FPData := @FDataArray[FActiveImage];
  end
  else
  begin
    FActiveImage := -1;
    FPData := nil
  end;
end;

function TMultiImage.PrepareInsert(Index, InsertCount: Integer): Boolean;
var
  I: Integer;
  OldImageCount, MoveCount: Integer;
begin
  OldImageCount := GetImageCount;

  // Inserting to empty image will add image at index 0
  if OldImageCount = 0 then
    Index := 0;

  if (Index >= 0) and (Index <= OldImageCount) and (InsertCount > 0) then
  begin
    SetLength(FDataArray, OldImageCount + InsertCount);
    if Index < OldImageCount then
    begin
      // Move images to new position
      MoveCount := OldImageCount - Index;
      System.Move(FDataArray[Index], FDataArray[Index + InsertCount], MoveCount * SizeOf(TImageData));
      // Null old images, not free them!
      for I := Index to Index + InsertCount - 1 do
        InitImage(FDataArray[I]);
    end;
    Result := True;
  end
  else
    Result := False;
end;

procedure TMultiImage.DoInsertImages(Index: Integer; const Images: TDynImageDataArray);
var
  I, Len: Integer;
begin
  Len := Length(Images);
  if PrepareInsert(Index, Len) then
  begin
    for I := 0 to Len - 1 do
      Imaging.CloneImage(Images[I], FDataArray[Index + I]);
  end;
end;

procedure TMultiImage.DoInsertNew(Index, AWidth, AHeight: Integer;
  AFormat: TImageFormat);
begin
  if PrepareInsert(Index, 1) then
    Imaging.NewImage(AWidth, AHeight, AFormat, FDataArray[Index]);
end;

procedure TMultiImage.Assign(Source: TPersistent);
var
  Arr: TDynImageDataArray;
begin
  if Source = nil then
  begin
    ClearAll;
  end
  else if Source is TMultiImage then
  begin
    AssignFromArray(TMultiImage(Source).FDataArray);
    SetActiveImage(TMultiImage(Source).ActiveImage);
  end
  else if Source is TSingleImage then
  begin
    SetLength(Arr, 1);
    Arr[0] := TSingleImage(Source).FImageData;
    AssignFromArray(Arr);
  end
  else
    inherited Assign(Source);
end;

procedure TMultiImage.AssignFromArray(const ADataArray: TDynImageDataArray);
var
  I: Integer;
begin
  Imaging.FreeImagesInArray(FDataArray);
  SetLength(FDataArray, Length(ADataArray));
  for I := 0 to GetImageCount - 1 do
  begin
    // Clone only valid images
    if Imaging.TestImage(ADataArray[I]) then
      Imaging.CloneImage(ADataArray[I], FDataArray[I])
    else
      Imaging.NewImage(DefaultWidth, DefaultHeight, ifDefault, FDataArray[I]);
  end;
  if GetImageCount > 0 then
    SetActiveImage(0);
end;

function TMultiImage.AddImage(AWidth, AHeight: Integer; AFormat: TImageFormat): Integer;
begin
  Result := GetImageCount;
  DoInsertNew(Result, AWidth, AHeight, AFormat);
end;

function TMultiImage.AddImage(const Image: TImageData): Integer;
begin
  Result := GetImageCount;
  DoInsertImages(Result, GetArrayFromImageData(Image));
end;

function TMultiImage.AddImage(Image: TBaseImage): Integer;
begin
  if Assigned(Image) and Image.Valid then
  begin
    Result := GetImageCount;
    DoInsertImages(Result, GetArrayFromImageData(Image.FPData^));
  end
  else
    Result := -1;
end;

procedure TMultiImage.AddImages(const Images: TDynImageDataArray);
begin
  DoInsertImages(GetImageCount, Images);
end;

procedure TMultiImage.AddImages(Images: TMultiImage);
begin
  DoInsertImages(GetImageCount, Images.FDataArray);
end;

procedure TMultiImage.InsertImage(Index, AWidth, AHeight: Integer;
  AFormat: TImageFormat);
begin
  DoInsertNew(Index, AWidth, AHeight, AFormat);
end;

procedure TMultiImage.InsertImage(Index: Integer; const Image: TImageData);
begin
  DoInsertImages(Index, GetArrayFromImageData(Image));
end;

procedure TMultiImage.InsertImage(Index: Integer; Image: TBaseImage);
begin
  if Assigned(Image) and Image.Valid then
    DoInsertImages(Index, GetArrayFromImageData(Image.FPData^));
end;

procedure TMultiImage.InsertImages(Index: Integer;
  const Images: TDynImageDataArray);
begin
  DoInsertImages(Index, Images);
end;

procedure TMultiImage.InsertImages(Index: Integer; Images: TMultiImage);
begin
  DoInsertImages(Index, Images.FDataArray);
end;

procedure TMultiImage.ExchangeImages(Index1, Index2: Integer);
var
  TempData: TImageData;
begin
  if (Index1 >= 0) and (Index1 < GetImageCount) and
     (Index2 >= 0) and (Index2 < GetImageCount) then
  begin
    TempData := FDataArray[Index1];
    FDataArray[Index1] := FDataArray[Index2];
    FDataArray[Index2] := TempData;
  end;
end;

procedure TMultiImage.DeleteImage(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < GetImageCount) then
  begin
    // Free image at index to be deleted
    Imaging.FreeImage(FDataArray[Index]);
    if Index < GetImageCount - 1 then
    begin
      // Move images to new indices if necessary
      for I := Index to GetImageCount - 2 do
        FDataArray[I] := FDataArray[I + 1];
    end;
    // Set new array length and update pointer to active image
    SetLength(FDataArray, GetImageCount - 1);
    SetPointer;
  end;
end;

procedure TMultiImage.ClearAll;
begin
  ImageCount := 0;
end;

procedure TMultiImage.ConvertImages(Format: TImageFormat);
var
  I: Integer;
begin
  for I := 0 to GetImageCount - 1 do
    Imaging.ConvertImage(FDataArray[I], Format);
end;

procedure TMultiImage.ResizeImages(NewWidth, NewHeight: Integer;
  Filter: TResizeFilter);
var
  I: Integer;
begin
  for I := 0 to GetImageCount - 1 do
    Imaging.ResizeImage(FDataArray[I], NewWidth, NewHeight, Filter);
end;

procedure TMultiImage.ReverseImages;
var
  I: Integer;
begin
  for I := 0 to GetImageCount div 2 do
    ExchangeImages(I, GetImageCount - 1 - I);
end;

procedure TMultiImage.LoadFromFile(const FileName: string);
begin
  if GetImageCount = 0 then
    ImageCount := 1;
  inherited LoadFromFile(FileName);
end;

procedure TMultiImage.LoadFromStream(Stream: TStream);
begin
  if GetImageCount = 0 then
    ImageCount := 1;
  inherited LoadFromStream(Stream);
end;

procedure TMultiImage.LoadMultiFromFile(const FileName: string);
begin
  Imaging.LoadMultiImageFromFile(FileName, FDataArray);
  SetActiveImage(0);
end;

procedure TMultiImage.LoadMultiFromStream(Stream: TStream);
begin
  Imaging.LoadMultiImageFromStream(Stream, FDataArray);
  SetActiveImage(0);
end;

function TMultiImage.SaveMultiToFile(const FileName: string): Boolean;
begin
  Result := Imaging.SaveMultiImageToFile(FileName, FDataArray);
end;

function TMultiImage.SaveMultiToStream(const Ext: string; Stream: TStream): Boolean;
begin
  Result := Imaging.SaveMultiImageToStream(Ext, Stream, FDataArray);
end;

{
  File Notes (obsolete):

  -- 0.77.1 ---------------------------------------------------
    - Added TSingleImage.AssignFromData and TMultiImage.AssignFromArray
      as a replacement for constructors used as methods (that is
      compiler error in Delphi XE3).
    - Added TBaseImage.ResizeToFit method.
    - Changed TMultiImage to have default state with no images.
    - TMultiImage.AddImage now returns index of newly added image.
    - Fixed img index bug in TMultiImage.ResizeImages

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Added MapImageData method to TBaseImage
    - Added Empty property to TBaseImage.
    - Added Clear method to TBaseImage.
    - Added ScanlineSize property to TBaseImage.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Added TMultiImage.ReverseImages method.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added SwapChannels method to TBaseImage.
    - Added ReplaceColor method to TBaseImage.
    - Added ToString method to TBaseImage.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Inserting images to empty MultiImage will act as Add method.
    - MultiImages with empty arrays will now create one image when
      LoadFromFile or LoadFromStream is called.
    - Fixed bug that caused AVs when getting props like Width, Height, asn Size
      and when inlining was off. There was call to Iff but with inlining disabled
      params like FPData.Size were evaluated and when FPData was nil => AV.
    - Added many FPData validity checks to many methods. There were AVs
      when calling most methods on empty TMultiImage.
    - Added AllImagesValid property to TMultiImage.
    - Fixed memory leak in TMultiImage.CreateFromParams.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added ResizeImages method to TMultiImage
    - removed Ext parameter from various LoadFromStream methods, no
      longer needed
    - fixed various issues concerning ActiveImage of TMultiImage
      (it pointed to invalid location after some operations)   
    - most of property set/get methods are now inline
    - added PixelPointers property to TBaseImage
    - added Images default array property to TMultiImage
    - renamed methods in TMultiImage to contain 'Image' instead of 'Level'
    - added canvas support
    - added OnDataSizeChanged and OnPixelsChanged event to TBaseImage
    - renamed TSingleImage.NewImage to RecreateImageData, made public, and
      moved to TBaseImage

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added props PaletteEntries and ScanLine to TBaseImage
    - added new constructor to TBaseImage that take TBaseImage source
    - TMultiImage levels adding and inserting rewritten internally
    - added some new functions to TMultiImage: AddLevels, InsertLevels
    - added some new functions to TBaseImage: Flip, Mirror, Rotate,
      CopyRect, StretchRect
    - TBasicImage.Resize has now filter parameter
    - new stuff added to TMultiImage (DataArray prop, ConvertLevels)

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - added AddLevel, InsertLevel, ExchangeLevels and DeleteLevel
      methods to TMultiImage
    - added TBaseImage, TSingleImage and TMultiImage with initial
      members
}

end.

