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

{ This unit contains image format loaders/savers for Network Graphics image
  file formats PNG, MNG, and JNG.}
unit ImagingNetworkGraphics;
        
interface

{$I ImagingOptions.inc}

{ If MNG support is enabled we must make sure PNG and JNG are enabled too.}
{$IFNDEF DONT_LINK_MNG}
  {$UNDEF DONT_LINK_PNG}
  {$UNDEF DONT_LINK_JNG}
{$ENDIF}

uses
  Types, SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility, ImagingFormats, dzlib;

type
  { Basic class for Network Graphics file formats loaders/savers.}
  TNetworkGraphicsFileFormat = class(TImageFileFormat)
  protected
    FSignature: TChar8;
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
    FLossyCompression: LongBool;
    FLossyAlpha: LongBool;
    FQuality: LongInt;
    FProgressive: LongBool;
    FZLibStrategy: Integer;
    function GetSupportedFormats: TImageFormats; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
    procedure Define; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
  published
    { Sets precompression filter used when saving images with lossless compression.
      Allowed values are: 0 (none), 1 (sub), 2 (up), 3 (average), 4 (paeth),
      5 (use 0 for indexed/gray images and 4 for RGB/ARGB images),
      6 (adaptive filtering - use best filter for each scanline - very slow).
      Note that filters 3 and 4 are much slower than filters 1 and 2.
      Default value is 5.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { Sets ZLib compression level used when saving images with lossless compression.
      Allowed values are in range 0 (no compression) to 9 (best compression).
      Default value is 5.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
    { Specifies whether MNG animation frames are saved with lossy or lossless
      compression. Lossless frames are saved as PNG images and lossy frames are
      saved as JNG images. Allowed values are 0 (False) and 1 (True).
      Default value is 0.}
    property LossyCompression: LongBool read FLossyCompression write FLossyCompression;
    { Defines whether alpha channel of lossy MNG frames or JNG images
      is lossy compressed too. Allowed values are 0 (False) and 1 (True).
      Default value is 0.}
    property LossyAlpha: LongBool read FLossyAlpha write FLossyAlpha;
    { Specifies compression quality used when saving lossy MNG frames or JNG images.
      For details look at ImagingJpegQuality option.}
    property Quality: LongInt read FQuality write FQuality;
    { Specifies whether images are saved in progressive format when saving lossy
      MNG frames or JNG images. For details look at ImagingJpegProgressive.}
    property Progressive: LongBool read FProgressive write FProgressive;
  end;

  { Class for loading Portable Network Graphics Images.
    Loads all types of this image format (all images in png test suite)
    and saves all types with bitcount >= 8 (non-interlaced only).
    Compression level and  filtering can be set by options interface.

    Supported ancillary chunks (loading):
    tRNS, bKGD
    (for indexed images transparency contains alpha values for palette,
    RGB/Gray images with transparency are converted to formats with alpha
    and pixels with transparent color are replaced with background color
    with alpha = 0).}
  TPNGFileFormat = class(TNetworkGraphicsFileFormat)
  private
    FLoadAnimated: LongBool;
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
  published
    property LoadAnimated: LongBool read FLoadAnimated write FLoadAnimated;
  end;

{$IFNDEF DONT_LINK_MNG}
  { Class for loading Multiple Network Graphics files.
    This format has complex animation capabilities but Imaging only
    extracts frames. Individual frames are stored as standard PNG or JNG
    images. Loads all types of these frames stored in IHDR-IEND and
    JHDR-IEND streams (Note that there are MNG chunks
    like BASI which define images but does not contain image data itself,
    those are ignored).
    Imaging saves MNG files as MNG-VLC (very low complexity) so it is basically
    an array of image frames without MNG animation chunks. Frames can be saved
    as lossless PNG or lossy JNG images (look at TPNGFileFormat and
    TJNGFileFormat for info). Every frame can be in different data format.
    
    Many frame compression settings can be modified by options interface.}
  TMNGFileFormat = class(TNetworkGraphicsFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
  end;
{$ENDIF}  

{$IFNDEF DONT_LINK_JNG}
  { Class for loading JPEG Network Graphics Images.
    Loads all types of this image format (all images in jng test suite)
    and saves all types except 12 bit JPEGs.
    Alpha channel in JNG images is stored separately from color/gray data and
    can be lossy (as JPEG image) or lossless (as PNG image) compressed.
    Type of alpha compression, compression level and quality,
    and filtering can be set by options interface.

    Supported ancillary chunks (loading):
    tRNS, bKGD
    (Images with transparency are converted to formats with alpha
    and pixels with transparent color are replaced with background color
    with alpha = 0).}
  TJNGFileFormat = class(TNetworkGraphicsFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
  end;
{$ENDIF}


implementation

uses
{$IFNDEF DONT_LINK_JNG}
  ImagingJpeg, ImagingIO,
{$ENDIF}
  ImagingCanvases;

const
  NGDefaultPreFilter = 5;
  NGDefaultCompressLevel = 5;
  NGDefaultLossyAlpha = False;
  NGDefaultLossyCompression = False;
  NGDefaultProgressive = False;
  NGDefaultQuality = 90;
  NGLosslessFormats: TImageFormats = [ifIndex8, ifGray8, ifA8Gray8, ifGray16,
    ifA16Gray16, ifR8G8B8, ifA8R8G8B8, ifR16G16B16, ifA16R16G16B16, ifB16G16R16,
    ifA16B16G16R16, ifBinary];
  NGLossyFormats: TImageFormats = [ifGray8, ifA8Gray8, ifR8G8B8, ifA8R8G8B8];
  PNGDefaultLoadAnimated = True;
  NGDefaultZLibStrategy = 1; // Z_FILTERED

  SPNGFormatName = 'Portable Network Graphics';
  SPNGMasks      = '*.png';
  SMNGFormatName = 'Multiple Network Graphics';
  SMNGMasks      = '*.mng';
  SJNGFormatName = 'JPEG Network Graphics';
  SJNGMasks      = '*.jng';

resourcestring
  SErrorLoadingChunk = 'Error when reading %s chunk data. File may be corrupted.';

type
  { Chunk header.}
  TChunkHeader = packed record
    DataSize: UInt32;
    ChunkID: TChar4;
  end;

  { IHDR chunk format - PNG header.}
  TIHDR = packed record
    Width: UInt32;                // Image width
    Height: UInt32;               // Image height
    BitDepth: Byte;               // Bits per pixel or bits per sample (for truecolor)
    ColorType: Byte;              // 0 = grayscale, 2 = truecolor, 3 = palette,
                                  // 4 = gray + alpha, 6 = truecolor + alpha
    Compression: Byte;            // Compression type:  0 = ZLib
    Filter: Byte;                 // Used precompress filter
    Interlacing: Byte;            // Used interlacing: 0 = no int, 1 = Adam7
  end;
  PIHDR = ^TIHDR;

  { MHDR chunk format - MNG header.}
  TMHDR = packed record
    FrameWidth: UInt32;         // Frame width
    FrameHeight: UInt32;        // Frame height
    TicksPerSecond: UInt32;     // FPS of animation
    NominalLayerCount: UInt32;  // Number of layers in file
    NominalFrameCount: UInt32;  // Number of frames in file
    NominalPlayTime: UInt32;    // Play time of animation in ticks
    SimplicityProfile: UInt32;  // Defines which MNG features are used in this file
  end;
  PMHDR = ^TMHDR;

  { JHDR chunk format - JNG header.}
  TJHDR = packed record
    Width: UInt32;                // Image width
    Height: UInt32;               // Image height
    ColorType: Byte;              // 8 = grayscale (Y), 10 = color (YCbCr),
                                  // 12 = gray + alpha (Y-alpha), 14 = color + alpha (YCbCr-alpha)
    SampleDepth: Byte;            // 8, 12 or 20 (8 and 12 samples together) bit
    Compression: Byte;            // Compression type:  8 = Huffman coding
    Interlacing: Byte;            // 0 = single scan, 8 = progressive
    AlphaSampleDepth: Byte;       // 0, 1, 2, 4, 8, 16 if alpha compression is 0 (PNG)
                                  // 8 if alpha compression is 8 (JNG)
    AlphaCompression: Byte;       // 0 = PNG grayscale IDAT, 8 = grayscale 8-bit JPEG
    AlphaFilter: Byte;            // 0 = PNG filter or no filter (JPEG)
    AlphaInterlacing: Byte;       // 0 = non interlaced
  end;
  PJHDR = ^TJHDR;

  { acTL chunk format - APNG animation control.}
  TacTL = packed record
    NumFrames: UInt32;          // Number of frames
    NumPlay: UInt32;            // Number of times to loop the animation (0 = inf)
  end;
  PacTL =^TacTL;

  { fcTL chunk format - APNG frame control.}
  TfcTL = packed record
    SeqNumber: UInt32;            // Sequence number of the animation chunk, starting from 0
    Width: UInt32;                // Width of the following frame
    Height: UInt32;               // Height of the following frame
    XOffset: UInt32;              // X position at which to render the following frame
    YOffset: UInt32;              // Y position at which to render the following frame
    DelayNumer: Word;             // Frame delay fraction numerator
    DelayDenom: Word;             // Frame delay fraction denominator
    DisposeOp: Byte;              // Type of frame area disposal to be done after rendering this frame
    BlendOp: Byte;                // Type of frame area rendering for this frame
  end;
  PfcTL = ^TfcTL;

  { pHYs chunk format - encodes the absolute or relative dimensions of pixels.}
  TpHYs = packed record
    PixelsPerUnitX: UInt32;
    PixelsPerUnitY: UInt32;
    UnitSpecifier: Byte;
  end;
  PpHYs = ^TpHYs;

const
  { PNG file identifier.}
  PNGSignature: TChar8 = #$89'PNG'#$0D#$0A#$1A#$0A;
  { MNG file identifier.}
  MNGSignature: TChar8 = #$8A'MNG'#$0D#$0A#$1A#$0A;
  { JNG file identifier.}
  JNGSignature: TChar8 = #$8B'JNG'#$0D#$0A#$1A#$0A;

  { Constants for chunk identifiers and signature identifiers.
    They are in big-endian format.}
  IHDRChunk: TChar4 = 'IHDR';
  IENDChunk: TChar4 = 'IEND';
  MHDRChunk: TChar4 = 'MHDR';
  MENDChunk: TChar4 = 'MEND';
  JHDRChunk: TChar4 = 'JHDR';
  IDATChunk: TChar4 = 'IDAT';
  JDATChunk: TChar4 = 'JDAT';
  JDAAChunk: TChar4 = 'JDAA';
  JSEPChunk: TChar4 = 'JSEP';
  PLTEChunk: TChar4 = 'PLTE';
  BACKChunk: TChar4 = 'BACK';
  DEFIChunk: TChar4 = 'DEFI';
  TERMChunk: TChar4 = 'TERM';
  tRNSChunk: TChar4 = 'tRNS';
  bKGDChunk: TChar4 = 'bKGD';
  gAMAChunk: TChar4 = 'gAMA';
  acTLChunk: TChar4 = 'acTL';
  fcTLChunk: TChar4 = 'fcTL';
  fdATChunk: TChar4 = 'fdAT';
  pHYsChunk: TChar4 = 'pHYs';

  { APNG frame dispose operations.}
  DisposeOpNone       = 0;
  DisposeOpBackground = 1;
  DisposeOpPrevious   = 2;

  { APNG frame blending modes}
  BlendOpSource = 0;
  BlendOpOver   = 1;

  { Interlace start and offsets.}
  RowStart: array[0..6] of LongInt = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of LongInt = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of LongInt = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of LongInt = (8, 8, 4, 4, 2, 2, 1);

type
  { Helper class that holds information about MNG frame in PNG or JNG format.}
  TFrameInfo = class
  public
    Index: Integer;
    FrameWidth, FrameHeight: LongInt;
    IsJpegFrame: Boolean;
    IHDR: TIHDR;
    JHDR: TJHDR;
    fcTL: TfcTL;
    pHYs: TpHYs;
    Palette: PPalette24;
    PaletteEntries: LongInt;
    Transparency: Pointer;
    TransparencySize: LongInt;
    Background: Pointer;
    BackgroundSize: LongInt;
    IDATMemory: TMemoryStream;
    JDATMemory: TMemoryStream;
    JDAAMemory: TMemoryStream;
    constructor Create(AIndex: Integer);
    destructor Destroy; override;
    procedure AssignSharedProps(Source: TFrameInfo);
  end;

  { Defines type of Network Graphics file.}
  TNGFileType = (ngPNG, ngAPNG, ngMNG, ngJNG);

  TNGFileHandler = class
  public
    FileFormat: TNetworkGraphicsFileFormat;
    FileType: TNGFileType;
    Frames: array of TFrameInfo;
    MHDR: TMHDR; // Main header for MNG files
    acTL: TacTL; // Global anim control for APNG files
    GlobalPalette: PPalette24;
    GlobalPaletteEntries: LongInt;
    GlobalTransparency: Pointer;
    GlobalTransparencySize: LongInt;
    constructor Create(AFileFormat: TNetworkGraphicsFileFormat);
    destructor Destroy; override;
    procedure Clear;
    function GetLastFrame: TFrameInfo;
    function AddFrameInfo: TFrameInfo;
    procedure LoadMetaData;
  end;

  { Network Graphics file parser and frame converter.}
  TNGFileLoader = class(TNGFileHandler)
  public
    function LoadFile(Handle: TImagingHandle): Boolean;
    procedure LoadImageFromPNGFrame(FrameWidth, FrameHeight: LongInt; const IHDR: TIHDR; IDATStream: TMemoryStream; var Image: TImageData);
{$IFNDEF DONT_LINK_JNG}
    procedure LoadImageFromJNGFrame(FrameWidth, FrameHeight: LongInt; const JHDR: TJHDR; IDATStream, JDATStream, JDAAStream: TMemoryStream; var Image: TImageData);
{$ENDIF}
    procedure ApplyFrameSettings(Frame: TFrameInfo; var Image: TImageData);
  end;

  TNGFileSaver = class(TNGFileHandler)
  public
    PreFilter: LongInt;
    CompressLevel: LongInt;
    LossyAlpha: Boolean;
    Quality: LongInt;
    Progressive: Boolean;
    ZLibStrategy: Integer;
    function SaveFile(Handle: TImagingHandle): Boolean;
    procedure AddFrame(const Image: TImageData; IsJpegFrame: Boolean);
    procedure StoreImageToPNGFrame(const IHDR: TIHDR; Bits: Pointer; FmtInfo: TImageFormatInfo; IDATStream: TMemoryStream);
{$IFNDEF DONT_LINK_JNG}
    procedure StoreImageToJNGFrame(const JHDR: TJHDR; const Image: TImageData; IDATStream, JDATStream, JDAAStream: TMemoryStream);
{$ENDIF}
    procedure SetFileOptions;
  end;

{$IFNDEF DONT_LINK_JNG}
  TCustomIOJpegFileFormat = class(TJpegFileFormat)
  protected
    FCustomIO: TIOFunctions;
    procedure SetJpegIO(const JpegIO: TIOFunctions); override;
    procedure SetCustomIO(const CustomIO: TIOFunctions);
  end;
{$ENDIF}

  TAPNGAnimator = class
  public
    class procedure Animate(var Images: TDynImageDataArray; const acTL: TacTL; const SrcFrames: array of TFrameInfo);
  end;

{ Helper routines }

function PaethPredictor(A, B, C: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  P, PA, PB, PC: LongInt;
begin
  P := A + B - C;
  PA := Abs(P - A);
  PB := Abs(P - B);
  PC := Abs(P - C);
  if (PA <= PB) and (PA <= PC) then
    Result := A
  else
    if PB <= PC then
      Result := B
    else
      Result := C;
end;

procedure SwapRGB(Line: PByte; Width, SampleDepth, BytesPerPixel: LongInt);
var
  I: LongInt;
  Tmp: Word;
begin
  case SampleDepth of
    8:
      for I := 0 to Width - 1 do
      with PColor24Rec(Line)^ do
      begin
        Tmp := R;
        R := B;
        B := Tmp;
        Inc(Line, BytesPerPixel);
      end;
    16:
      for I := 0 to Width - 1 do
      with PColor48Rec(Line)^ do
      begin
        Tmp := R;
        R := B;
        B := Tmp;
        Inc(Line, BytesPerPixel);
      end;
    end;
 end;

{$IFNDEF DONT_LINK_JNG}

{ TCustomIOJpegFileFormat class implementation }

procedure TCustomIOJpegFileFormat.SetCustomIO(const CustomIO: TIOFunctions);
begin
  FCustomIO := CustomIO;
end;

procedure TCustomIOJpegFileFormat.SetJpegIO(const JpegIO: TIOFunctions);
begin
  inherited SetJpegIO(FCustomIO);
end;

{$ENDIF}

{ TFrameInfo class implementation }

constructor TFrameInfo.Create(AIndex: Integer);
begin
  Index := AIndex;
  IDATMemory := TMemoryStream.Create;
  JDATMemory := TMemoryStream.Create;
  JDAAMemory := TMemoryStream.Create;
end;

destructor TFrameInfo.Destroy;
begin
  FreeMem(Palette);
  FreeMem(Transparency);
  FreeMem(Background);
  IDATMemory.Free;
  JDATMemory.Free;
  JDAAMemory.Free;
  inherited Destroy;
end;

procedure TFrameInfo.AssignSharedProps(Source: TFrameInfo);
begin
  IHDR := Source.IHDR;
  JHDR := Source.JHDR;
  PaletteEntries := Source.PaletteEntries;
  GetMem(Palette, PaletteEntries * SizeOf(TColor24Rec));
  Move(Source.Palette^, Palette^, PaletteEntries * SizeOf(TColor24Rec));
  TransparencySize := Source.TransparencySize;
  GetMem(Transparency, TransparencySize);
  Move(Source.Transparency^, Transparency^, TransparencySize);
end;

{ TNGFileHandler class implementation}

destructor TNGFileHandler.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TNGFileHandler.Clear;
var
  I: LongInt;
begin
  for I := 0 to Length(Frames) - 1 do
    Frames[I].Free;
  SetLength(Frames, 0);
  FreeMemNil(GlobalPalette);
  GlobalPaletteEntries := 0;
  FreeMemNil(GlobalTransparency);
  GlobalTransparencySize := 0;
end;

constructor TNGFileHandler.Create(AFileFormat: TNetworkGraphicsFileFormat);
begin
  FileFormat := AFileFormat;
end;

function TNGFileHandler.GetLastFrame: TFrameInfo;
var
  Len: LongInt;
begin
  Len := Length(Frames);
  if Len > 0 then
    Result := Frames[Len - 1]
  else
    Result := nil;
end;

procedure TNGFileHandler.LoadMetaData;
var
  I: Integer;
  Delay, Denom: Integer;
begin
  if FileType = ngAPNG then
  begin
    // Num plays of APNG animation
    FileFormat.FMetadata.SetMetaItem(SMetaAnimationLoops, acTL.NumPlay);
  end;

  for I := 0 to High(Frames) do
  begin
    if Frames[I].pHYs.UnitSpecifier = 1 then
    begin
      // Store physical pixel dimensions, in PNG stored as pixels per meter DPM
      FileFormat.FMetadata.SetPhysicalPixelSize(ruDpm, Frames[I].pHYs.PixelsPerUnitX,
        Frames[I].pHYs.PixelsPerUnitY);
    end;
    if FileType = ngAPNG then
    begin
      // Store frame delay of APNG file frame
      Denom := Frames[I].fcTL.DelayDenom;
      if Denom = 0 then
        Denom := 100;
      Delay := Round(1000 * (Frames[I].fcTL.DelayNumer / Denom));
      FileFormat.FMetadata.SetMetaItem(SMetaFrameDelay, Delay, I);
    end;
  end;
end;

function TNGFileHandler.AddFrameInfo: TFrameInfo;
var
  Len: LongInt;
begin
  Len := Length(Frames);
  SetLength(Frames, Len + 1);
  Result := TFrameInfo.Create(Len);
  Frames[Len] := Result;
end;

{ TNGFileLoader class implementation}

function TNGFileLoader.LoadFile(Handle: TImagingHandle): Boolean;
var
  Sig: TChar8;
  Chunk: TChunkHeader;
  ChunkData: Pointer;
  ChunkCrc: UInt32;

  procedure ReadChunk;
  begin
    GetIO.Read(Handle, @Chunk, SizeOf(Chunk));
    Chunk.DataSize := SwapEndianUInt32(Chunk.DataSize);
  end;

  procedure ReadChunkData;
  var
    ReadBytes: UInt32;
  begin
    FreeMemNil(ChunkData);
    GetMem(ChunkData, Chunk.DataSize);
    ReadBytes := GetIO.Read(Handle, ChunkData, Chunk.DataSize);
    GetIO.Read(Handle, @ChunkCrc, SizeOf(ChunkCrc));
    if ReadBytes <> Chunk.DataSize then
      RaiseImaging(SErrorLoadingChunk, [string(Chunk.ChunkID)]);
  end;

  procedure SkipChunkData;
  begin
    GetIO.Seek(Handle, Chunk.DataSize + SizeOf(ChunkCrc), smFromCurrent);
  end;

  procedure StartNewPNGImage;
  var
    Frame: TFrameInfo;
  begin
    ReadChunkData;

    if Chunk.ChunkID = fcTLChunk then
    begin
      if (Length(Frames) = 1) and (Frames[0].IDATMemory.Size = 0) then
      begin
        // First fcTL chunk maybe for first IDAT frame which is alredy created
        Frame := Frames[0];
      end
      else
      begin
        // Subsequent APNG frames with data in fdAT
        Frame := AddFrameInfo;
        // Copy some shared props from first frame (IHDR is the same for all APNG frames, palette etc)
        Frame.AssignSharedProps(Frames[0]);
      end;
      Frame.fcTL := PfcTL(ChunkData)^;
      SwapEndianUInt32(@Frame.fcTL, 5);
      Frame.fcTL.DelayNumer := SwapEndianWord(Frame.fcTL.DelayNumer);
      Frame.fcTL.DelayDenom := SwapEndianWord(Frame.fcTL.DelayDenom);
      Frame.FrameWidth := Frame.fcTL.Width;
      Frame.FrameHeight := Frame.fcTL.Height;
    end
    else
    begin
      // This is frame defined by IHDR chunk
      Frame := AddFrameInfo;
      Frame.IHDR := PIHDR(ChunkData)^;
      SwapEndianUInt32(@Frame.IHDR, 2);
      Frame.FrameWidth := Frame.IHDR.Width;
      Frame.FrameHeight := Frame.IHDR.Height;
    end;
    Frame.IsJpegFrame := False;
  end;

  procedure StartNewJNGImage;
  var
    Frame: TFrameInfo;
  begin
    ReadChunkData;
    Frame := AddFrameInfo;
    Frame.IsJpegFrame := True;
    Frame.JHDR := PJHDR(ChunkData)^;
    SwapEndianUInt32(@Frame.JHDR, 2);
    Frame.FrameWidth := Frame.JHDR.Width;
    Frame.FrameHeight := Frame.JHDR.Height;
  end;

  procedure AppendIDAT;
  begin
    ReadChunkData;
    // Append current IDAT/fdAT chunk to storage stream
    if Chunk.ChunkID = IDATChunk then
      GetLastFrame.IDATMemory.Write(ChunkData^, Chunk.DataSize)
    else if Chunk.ChunkID = fdATChunk then
      GetLastFrame.IDATMemory.Write(PByteArray(ChunkData)[4], Chunk.DataSize - SizeOf(UInt32));
  end;

  procedure AppendJDAT;
  begin
    ReadChunkData;
    // Append current JDAT chunk to storage stream
    GetLastFrame.JDATMemory.Write(ChunkData^, Chunk.DataSize);
  end;

  procedure AppendJDAA;
  begin
    ReadChunkData;
    // Append current JDAA chunk to storage stream
    GetLastFrame.JDAAMemory.Write(ChunkData^, Chunk.DataSize);
  end;

  procedure LoadPLTE;
  begin
    ReadChunkData;
    if GetLastFrame = nil then
    begin
      // Load global palette
      GetMem(GlobalPalette, Chunk.DataSize);
      Move(ChunkData^, GlobalPalette^, Chunk.DataSize);
      GlobalPaletteEntries := Chunk.DataSize div 3;
    end
    else if GetLastFrame.Palette = nil then
    begin
      if (Chunk.DataSize = 0) and (GlobalPalette <> nil) then
      begin
        // Use global palette
        GetMem(GetLastFrame.Palette, GlobalPaletteEntries * SizeOf(TColor24Rec));
        Move(GlobalPalette^, GetLastFrame.Palette^, GlobalPaletteEntries * SizeOf(TColor24Rec));
        GetLastFrame.PaletteEntries := GlobalPaletteEntries;
      end
      else
      begin
        // Load pal from PLTE chunk
        GetMem(GetLastFrame.Palette, Chunk.DataSize);
        Move(ChunkData^, GetLastFrame.Palette^, Chunk.DataSize);
        GetLastFrame.PaletteEntries := Chunk.DataSize div 3;
      end;
    end;
  end;

  procedure LoadtRNS;
  begin
    ReadChunkData;
    if GetLastFrame = nil then
    begin
      // Load global transparency
      GetMem(GlobalTransparency, Chunk.DataSize);
      Move(ChunkData^, GlobalTransparency^, Chunk.DataSize);
      GlobalTransparencySize := Chunk.DataSize;
    end
    else if GetLastFrame.Transparency = nil then
    begin
      if (Chunk.DataSize = 0) and (GlobalTransparency <> nil) then
      begin
        // Use global transparency
        GetMem(GetLastFrame.Transparency, GlobalTransparencySize);
        Move(GlobalTransparency^, GetLastFrame.Transparency^, Chunk.DataSize);
        GetLastFrame.TransparencySize := GlobalTransparencySize;
      end
      else
      begin
        // Load pal from tRNS chunk
        GetMem(GetLastFrame.Transparency, Chunk.DataSize);
        Move(ChunkData^, GetLastFrame.Transparency^, Chunk.DataSize);
        GetLastFrame.TransparencySize := Chunk.DataSize;
      end;
    end;
  end;

  procedure LoadbKGD;
  begin
    ReadChunkData;
    if GetLastFrame.Background = nil then
    begin
      GetMem(GetLastFrame.Background, Chunk.DataSize);
      Move(ChunkData^, GetLastFrame.Background^, Chunk.DataSize);
      GetLastFrame.BackgroundSize := Chunk.DataSize;
    end;
  end;

  procedure HandleacTL;
  begin
    FileType := ngAPNG;
    ReadChunkData;
    acTL := PacTL(ChunkData)^;
    SwapEndianUInt32(@acTL, SizeOf(acTL) div SizeOf(UInt32));
  end;

  procedure LoadpHYs;
  begin
    ReadChunkData;
    with GetLastFrame do
    begin
      pHYs := PpHYs(ChunkData)^;
      SwapEndianUInt32(@pHYs, SizeOf(pHYs) div SizeOf(UInt32));
    end;
  end;

begin
  Result := False;
  Clear;
  ChunkData := nil;
  with GetIO do
  try
    Read(Handle, @Sig, SizeOf(Sig));
    // Set file type according to the signature
    if Sig = PNGSignature then FileType := ngPNG
    else if Sig = MNGSignature then FileType := ngMNG
    else if Sig = JNGSignature then FileType := ngJNG
    else Exit;

    if FileType = ngMNG then
    begin
      // Store MNG header if present
      ReadChunk;
      ReadChunkData;
      MHDR := PMHDR(ChunkData)^;
      SwapEndianUInt32(@MHDR, SizeOf(MHDR) div SizeOf(UInt32));
    end;

    // Read chunks until ending chunk or EOF is reached
    repeat
      ReadChunk;
      if (Chunk.ChunkID = IHDRChunk) or (Chunk.ChunkID = fcTLChunk) then StartNewPNGImage
      else if Chunk.ChunkID = JHDRChunk then StartNewJNGImage
      else if (Chunk.ChunkID = IDATChunk) or (Chunk.ChunkID = fdATChunk) then AppendIDAT
      else if Chunk.ChunkID = JDATChunk then AppendJDAT
      else if Chunk.ChunkID = JDAAChunk then AppendJDAA
      else if Chunk.ChunkID = PLTEChunk then LoadPLTE
      else if Chunk.ChunkID = tRNSChunk then LoadtRNS
      else if Chunk.ChunkID = bKGDChunk then LoadbKGD
      else if Chunk.ChunkID = acTLChunk then HandleacTL
      else if Chunk.ChunkID = pHYsChunk then LoadpHYs
      else SkipChunkData;
    until Eof(Handle) or (Chunk.ChunkID = MENDChunk) or
      ((FileType <> ngMNG) and (Chunk.ChunkID = IENDChunk));

    Result := True;
  finally
    FreeMemNil(ChunkData);
  end;
end;

procedure TNGFileLoader.LoadImageFromPNGFrame(FrameWidth, FrameHeight: LongInt; const IHDR: TIHDR;
  IDATStream: TMemoryStream; var Image: TImageData);
type
  TGetPixelFunc = function(Line: PByteArray; X: LongInt): Byte;
var
  LineBuffer: array[Boolean] of PByteArray;
  ActLine: Boolean;
  Data, TotalBuffer, ZeroLine, PrevLine: Pointer;
  BitCount, TotalPos, BytesPerPixel, I, Pass,
  SrcDataSize, BytesPerLine, InterlaceLineBytes, InterlaceWidth: LongInt;
  TotalSize: Integer;
  Info: TImageFormatInfo;

  procedure DecodeAdam7;
  const
    BitTable: array[1..8] of LongInt = ($1, $3, 0, $F, 0, 0, 0, $FF);
    StartBit: array[1..8] of LongInt = (7, 6, 0, 4, 0, 0, 0, 0);
  var
    Src, Dst, Dst2: PByte;
    CurBit, Col: LongInt;
  begin
    Src := @LineBuffer[ActLine][1];
    Col := ColumnStart[Pass];
    with Image do
      case BitCount of
        1, 2, 4:
          begin
            Dst := @PByteArray(Data)[I * BytesPerLine];
            repeat
              CurBit := StartBit[BitCount];
              repeat
                Dst2 := @PByteArray(Dst)[(BitCount * Col) shr 3];
                Dst2^ := Dst2^ or ((Src^ shr CurBit) and BitTable[BitCount])
                  shl (StartBit[BitCount] - (Col * BitCount mod 8));
                Inc(Col, ColumnIncrement[Pass]);
                Dec(CurBit, BitCount);
              until CurBit < 0;
              Inc(Src);
            until Col >= Width;
          end;
        else
        begin
          Dst := @PByteArray(Data)[I * BytesPerLine + Col * BytesPerPixel];
          repeat
            CopyPixel(Src, Dst, BytesPerPixel);
            Inc(Dst, BytesPerPixel);
            Inc(Src, BytesPerPixel);
            Inc(Dst, ColumnIncrement[Pass] * BytesPerPixel - BytesPerPixel);
            Inc(Col, ColumnIncrement[Pass]);
          until Col >= Width;
        end;
      end;
  end;

  procedure FilterScanline(Filter: Byte; BytesPerPixel: LongInt; Line, PrevLine, Target: PByteArray;
    BytesPerLine: LongInt);
  var
    I: LongInt;
  begin
    case Filter of
      0:
        begin
          // No filter
          Move(Line^, Target^, BytesPerLine);
        end;
      1:
        begin
          // Sub filter
          Move(Line^, Target^, BytesPerPixel);
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] + Target[I - BytesPerPixel]) and $FF;
        end;
      2:
        begin
          // Up filter
          for I := 0 to BytesPerLine - 1 do
            Target[I] := (Line[I] + PrevLine[I]) and $FF;
        end;
      3:
        begin
          // Average filter
          for I := 0 to BytesPerPixel - 1 do
            Target[I] := (Line[I] + PrevLine[I] shr 1) and $FF;
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] + (Target[I - BytesPerPixel] + PrevLine[I]) shr 1) and $FF;
        end;
      4:
        begin
          // Paeth filter
          for I := 0 to BytesPerPixel - 1 do
            Target[I] := (Line[I] + PaethPredictor(0, PrevLine[I], 0)) and $FF;
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] + PaethPredictor(Target[I - BytesPerPixel], PrevLine[I], PrevLine[I - BytesPerPixel])) and $FF;
        end;
    end;
  end;

  procedure TransformLOCOToRGB(Data: PByte; NumPixels, BytesPerPixel: LongInt);
  var
    I: LongInt;
  begin
    for I := 0 to NumPixels - 1 do
    begin
      if IHDR.BitDepth = 8 then
      begin
        PColor32Rec(Data).R := Byte(PColor32Rec(Data).R + PColor32Rec(Data).G);
        PColor32Rec(Data).B := Byte(PColor32Rec(Data).B + PColor32Rec(Data).G);
      end
      else
      begin
        PColor64Rec(Data).R := Word(PColor64Rec(Data).R + PColor64Rec(Data).G);
        PColor64Rec(Data).B := Word(PColor64Rec(Data).B + PColor64Rec(Data).G);
      end;
      Inc(Data, BytesPerPixel);
    end;
  end;

  function CheckBinaryPalette: Boolean;
  begin
    with GetLastFrame do
      Result := (PaletteEntries = 2) and
        (Palette[0].R = 0) and (Palette[0].G = 0) and (Palette[0].B = 0) and
        (Palette[1].R = 255) and (Palette[1].G = 255) and (Palette[1].B = 255);
  end;

begin
  Image.Width := FrameWidth;
  Image.Height := FrameHeight;
  Image.Format := ifUnknown;

  case IHDR.ColorType of
    0:
      begin
        // Gray scale image
        case IHDR.BitDepth of
          1:       Image.Format := ifBinary;
          2, 4, 8: Image.Format := ifGray8;
          16:      Image.Format := ifGray16;
        end;
        BitCount := IHDR.BitDepth;
      end;
    2:
      begin
        // RGB image
        case IHDR.BitDepth of
          8:  Image.Format := ifR8G8B8;
          16: Image.Format := ifR16G16B16;
        end;
        BitCount := IHDR.BitDepth * 3;
      end;
    3:
      begin
        // Indexed image
        if (IHDR.BitDepth = 1) and CheckBinaryPalette  then
          Image.Format := ifBinary
        else
          Image.Format := ifIndex8;
        BitCount := IHDR.BitDepth;
      end;
    4:
      begin
        // Grayscale + alpha image
        case IHDR.BitDepth of
          8: Image.Format := ifA8Gray8;
          16: Image.Format := ifA16Gray16;
        end;
        BitCount := IHDR.BitDepth * 2;
      end;
    6:
      begin
        // ARGB image
        case IHDR.BitDepth of
          8: Image.Format := ifA8R8G8B8;
          16: Image.Format := ifA16R16G16B16;
        end;
        BitCount := IHDR.BitDepth * 4;
      end;
  end;

  GetImageFormatInfo(Image.Format, Info);
  BytesPerPixel := (BitCount + 7) div 8;

  LineBuffer[True] := nil;
  LineBuffer[False] := nil;
  TotalBuffer := nil;
  ZeroLine := nil;
  ActLine := True;

  // Start decoding
  with Image do
  try
    BytesPerLine := (Width * BitCount + 7) div 8;
    SrcDataSize := Height * BytesPerLine;
    GetMem(Data, SrcDataSize);
    FillChar(Data^, SrcDataSize, 0);
    GetMem(ZeroLine, BytesPerLine);
    FillChar(ZeroLine^, BytesPerLine, 0);

    if IHDR.Interlacing = 1 then
    begin
      // Decode interlaced images
      TotalPos := 0;
      DecompressBuf(IDATStream.Memory, IDATStream.Size, 0,
        Pointer(TotalBuffer), TotalSize);
      GetMem(LineBuffer[True], BytesPerLine + 1);
      GetMem(LineBuffer[False], BytesPerLine + 1);
      for Pass := 0 to 6 do
      begin
        // Prepare next interlace run
        if Width <= ColumnStart[Pass] then
          Continue;
        InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 -
          ColumnStart[Pass]) div ColumnIncrement[Pass];
        InterlaceLineBytes := (InterlaceWidth * BitCount + 7) shr 3;
        I := RowStart[Pass];
        FillChar(LineBuffer[True][0], BytesPerLine + 1, 0);
        FillChar(LineBuffer[False][0], BytesPerLine + 1, 0);
        while I < Height do
        begin
          // Copy line from decompressed data to working buffer
          Move(PByteArray(TotalBuffer)[TotalPos],
            LineBuffer[ActLine][0], InterlaceLineBytes + 1);
          Inc(TotalPos, InterlaceLineBytes + 1);
          // Swap red and blue channels if necessary
          if (IHDR.ColorType in [2, 6]) then
            SwapRGB(@LineBuffer[ActLine][1], InterlaceWidth, IHDR.BitDepth, BytesPerPixel);
          // Reverse-filter current scanline
          FilterScanline(LineBuffer[ActLine][0], BytesPerPixel,
            @LineBuffer[ActLine][1], @LineBuffer[not ActLine][1],
            @LineBuffer[ActLine][1], InterlaceLineBytes);
          // Decode Adam7 interlacing
          DecodeAdam7;
          ActLine := not ActLine;
          // Continue with next row in interlaced order
          Inc(I, RowIncrement[Pass]);
        end;
      end;
    end
    else
    begin
      // Decode non-interlaced images
      PrevLine := ZeroLine;
      DecompressBuf(IDATStream.Memory, IDATStream.Size, SrcDataSize + Height,
        Pointer(TotalBuffer), TotalSize);
      for I := 0 to Height - 1 do
      begin
        // Swap red and blue channels if necessary
        if IHDR.ColorType in [2, 6] then
          SwapRGB(@PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1], Width,
           IHDR.BitDepth, BytesPerPixel);
        // reverse-filter current scanline
        FilterScanline(PByteArray(TotalBuffer)[I * (BytesPerLine + 1)],
          BytesPerPixel, @PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1],
          PrevLine, @PByteArray(Data)[I * BytesPerLine], BytesPerLine);
        PrevLine := @PByteArray(Data)[I * BytesPerLine];
      end;
    end;

    Size := Info.GetPixelsSize(Info.Format, Width, Height);

    if Size <> SrcDataSize then
    begin
      // If source data size is different from size of image in assigned
      // format we must convert it (it is in 1/2/4 bit count)
      GetMem(Bits, Size);
      case IHDR.BitDepth of
        1:
          begin
            // Convert only indexed, keep black and white in ifBinary
            if IHDR.ColorType <> 0 then
              Convert1To8(Data, Bits, Width, Height, BytesPerLine, False);
          end;
        2: Convert2To8(Data, Bits, Width, Height, BytesPerLine, IHDR.ColorType = 0);
        4: Convert4To8(Data, Bits, Width, Height, BytesPerLine, IHDR.ColorType = 0);
      end;
      FreeMem(Data);
    end
    else
    begin
      // If source data size is the same as size of
      // image Bits in assigned format we simply copy pointer reference
      Bits := Data;
    end;

    // LOCO transformation was used too (only for color types 2 and 6)
    if (IHDR.Filter = 64) and (IHDR.ColorType in [2, 6]) then
      TransformLOCOToRGB(Bits, Width * Height, BytesPerPixel);

    // Images with 16 bit channels must be swapped because of PNG's big endianity
    if IHDR.BitDepth = 16 then
      SwapEndianWord(Bits, Width * Height * BytesPerPixel div SizeOf(Word));
  finally
    FreeMem(LineBuffer[True]);
    FreeMem(LineBuffer[False]);
    FreeMem(TotalBuffer);
    FreeMem(ZeroLine);
  end;
end;

{$IFNDEF DONT_LINK_JNG}

procedure TNGFileLoader.LoadImageFromJNGFrame(FrameWidth, FrameHeight: LongInt; const JHDR: TJHDR; IDATStream,
  JDATStream, JDAAStream: TMemoryStream; var Image: TImageData);
var
  AlphaImage: TImageData;
  FakeIHDR: TIHDR;
  FmtInfo: TImageFormatInfo;
  I: LongInt;
  AlphaPtr: PByte;
  GrayPtr: PWordRec;
  ColorPtr: PColor32Rec;

  procedure LoadJpegFromStream(Stream: TStream; var DestImage: TImageData);
  var
    JpegFormat: TCustomIOJpegFileFormat;
    Handle: TImagingHandle;
    DynImages: TDynImageDataArray;
  begin
    if JHDR.SampleDepth <> 12 then
    begin
      JpegFormat := TCustomIOJpegFileFormat.Create;
      JpegFormat.SetCustomIO(StreamIO);
      Stream.Position := 0;
      Handle := StreamIO.Open(Pointer(Stream), omReadOnly);
      try
        JpegFormat.LoadData(Handle, DynImages, True);
        DestImage := DynImages[0];
      finally
        StreamIO.Close(Handle);
        JpegFormat.Free;
        SetLength(DynImages, 0);
      end;
    end
    else
      NewImage(FrameWidth, FrameHeight, ifR8G8B8, DestImage);
  end;

begin
  LoadJpegFromStream(JDATStream, Image);

  // If present separate alpha channel is processed
  if (JHDR.ColorType in [12, 14]) and (Image.Format in [ifGray8, ifR8G8B8]) then
  begin
    InitImage(AlphaImage);
    if JHDR.AlphaCompression = 0 then
    begin
      // Alpha channel is PNG compressed
      FakeIHDR.Width := JHDR.Width;
      FakeIHDR.Height := JHDR.Height;
      FakeIHDR.ColorType := 0;
      FakeIHDR.BitDepth := JHDR.AlphaSampleDepth;
      FakeIHDR.Filter := JHDR.AlphaFilter;
      FakeIHDR.Interlacing := JHDR.AlphaInterlacing;

      LoadImageFromPNGFrame(FrameWidth, FrameHeight, FakeIHDR, IDATStream, AlphaImage);
    end
    else
    begin
      // Alpha channel is JPEG compressed
      LoadJpegFromStream(JDAAStream, AlphaImage);
    end;

    // Check if alpha channel is the same size as image
    if (Image.Width <> AlphaImage.Width) and (Image.Height <> AlphaImage.Height) then
      ResizeImage(AlphaImage, Image.Width, Image.Height, rfNearest);

    // Check alpha channels data format
    GetImageFormatInfo(AlphaImage.Format, FmtInfo);
    if (FmtInfo.BytesPerPixel > 1) or (not FmtInfo.HasGrayChannel) then
      ConvertImage(AlphaImage, ifGray8);

    // Convert image to fromat with alpha channel
    if Image.Format = ifGray8 then
      ConvertImage(Image, ifA8Gray8)
    else
      ConvertImage(Image, ifA8R8G8B8);

    // Combine alpha channel with image
    AlphaPtr := AlphaImage.Bits;
    if Image.Format = ifA8Gray8 then
    begin
      GrayPtr := Image.Bits;
      for I := 0 to Image.Width * Image.Height - 1 do
      begin
        GrayPtr.High := AlphaPtr^;
        Inc(GrayPtr);
        Inc(AlphaPtr);
      end;
    end
    else
    begin
      ColorPtr := Image.Bits;
      for I := 0 to Image.Width * Image.Height - 1 do
      begin
        ColorPtr.A := AlphaPtr^;
        Inc(ColorPtr);
        Inc(AlphaPtr);
      end;
    end;

    FreeImage(AlphaImage);
  end;
end;

{$ENDIF}

procedure TNGFileLoader.ApplyFrameSettings(Frame: TFrameInfo; var Image: TImageData);
var
  FmtInfo: TImageFormatInfo;
  BackGroundColor: TColor64Rec;
  ColorKey: TColor64Rec;
  Alphas: PByteArray;
  AlphasSize: LongInt;
  IsColorKeyPresent: Boolean;
  IsBackGroundPresent: Boolean;
  IsColorFormat: Boolean;

  procedure ConverttRNS;
  begin
    if FmtInfo.IsIndexed then
    begin
      if Alphas = nil then
      begin
        GetMem(Alphas, Frame.TransparencySize);
        Move(Frame.Transparency^, Alphas^, Frame.TransparencySize);
        AlphasSize := Frame.TransparencySize;
      end;
    end
    else if not FmtInfo.HasAlphaChannel then
    begin
      FillChar(ColorKey, SizeOf(ColorKey), 0);
      Move(Frame.Transparency^, ColorKey, Min(Frame.TransparencySize, SizeOf(ColorKey)));
      if IsColorFormat then
        SwapValues(ColorKey.R, ColorKey.B);
      SwapEndianWord(@ColorKey, 3);
      // 1/2/4 bit images were converted to 8 bit so we must convert color key too
      if (not Frame.IsJpegFrame) and (Frame.IHDR.ColorType in [0, 4]) then
        case Frame.IHDR.BitDepth of
          1: ColorKey.B := Word(ColorKey.B * 255);
          2: ColorKey.B := Word(ColorKey.B * 85);
          4: ColorKey.B := Word(ColorKey.B * 17);
        end;
      IsColorKeyPresent := True;
    end;
  end;

  procedure ConvertbKGD;
  begin
    FillChar(BackGroundColor, SizeOf(BackGroundColor), 0);
    Move(Frame.Background^, BackGroundColor, Min(Frame.BackgroundSize, SizeOf(BackGroundColor)));
    if IsColorFormat then
      SwapValues(BackGroundColor.R, BackGroundColor.B);
    SwapEndianWord(@BackGroundColor, 3);
    // 1/2/4 bit images were converted to 8 bit so we must convert back color too
    if (not Frame.IsJpegFrame) and (Frame.IHDR.ColorType in [0, 4]) then
      case Frame.IHDR.BitDepth of
        1: BackGroundColor.B := Word(BackGroundColor.B * 255);
        2: BackGroundColor.B := Word(BackGroundColor.B * 85);
        4: BackGroundColor.B := Word(BackGroundColor.B * 17);
      end;
    IsBackGroundPresent := True;
  end;

  procedure ReconstructPalette;
  var
    I: LongInt;
  begin
    with Image do
    begin
      GetMem(Palette, FmtInfo.PaletteEntries * SizeOf(TColor32Rec));
      FillChar(Palette^, FmtInfo.PaletteEntries * SizeOf(TColor32Rec), $FF);
      // if RGB palette was loaded from file then use it
      if Frame.Palette <> nil then
        for I := 0 to Min(Frame.PaletteEntries, FmtInfo.PaletteEntries) - 1 do
        with Palette[I] do
        begin
          R := Frame.Palette[I].B;
          G := Frame.Palette[I].G;
          B := Frame.Palette[I].R;
        end;
      // if palette alphas were loaded from file then use them
      if Alphas <> nil then
      begin
        for I := 0 to Min(AlphasSize, FmtInfo.PaletteEntries) - 1 do
          Palette[I].A := Alphas[I];
      end;
    end;
  end;

  procedure ApplyColorKey;
  var
    DestFmt: TImageFormat;
    Col32, Bkg32: TColor32Rec;
    OldPixel, NewPixel: Pointer;
  begin
    case Image.Format of
      ifGray8: DestFmt := ifA8Gray8;
      ifGray16: DestFmt := ifA16Gray16;
      ifR8G8B8: DestFmt := ifA8R8G8B8;
      ifR16G16B16: DestFmt := ifA16R16G16B16;
    else
      DestFmt := ifUnknown;
    end;

    if DestFmt <> ifUnknown then
    begin
      if not IsBackGroundPresent then
        BackGroundColor := ColorKey;
      ConvertImage(Image, DestFmt);

      // Now back color and color key must be converted to image's data format, looks ugly
      case Image.Format of
        ifA8Gray8:
          begin
            Col32 := Color32(0, 0, $FF, Byte(ColorKey.B));
            Bkg32 := Color32(0, 0, 0, Byte(BackGroundColor.B));
          end;
        ifA16Gray16:
          begin
            ColorKey.G := $FFFF;
          end;
        ifA8R8G8B8:
          begin
            Col32 := Color32($FF, Byte(ColorKey.R), Byte(ColorKey.G), Byte(ColorKey.B));
            Bkg32 := Color32(0, Byte(BackGroundColor.R), Byte(BackGroundColor.G), Byte(BackGroundColor.B));
          end;
        ifA16R16G16B16:
          begin
            ColorKey.A := $FFFF;
          end;
      end;

      if Image.Format in [ifA8Gray8, ifA8R8G8B8] then
      begin
        OldPixel := @Col32;
        NewPixel := @Bkg32;
      end
      else
      begin
        OldPixel := @ColorKey;
        NewPixel := @BackGroundColor;
      end;

      ReplaceColor(Image, 0, 0, Image.Width, Image.Height, OldPixel, NewPixel);
    end;
  end;

begin
  Alphas := nil;
  IsColorKeyPresent := False;
  IsBackGroundPresent := False;
  GetImageFormatInfo(Image.Format, FmtInfo);

  IsColorFormat := (Frame.IsJpegFrame and (Frame.JHDR.ColorType in [10, 14])) or
    (not Frame.IsJpegFrame and (Frame.IHDR.ColorType in [2, 6]));

  // Convert some chunk data to useful format
  if Frame.TransparencySize > 0 then
    ConverttRNS;
  if Frame.BackgroundSize > 0 then
    ConvertbKGD;

  // Build palette for indexed images
  if FmtInfo.IsIndexed then
    ReconstructPalette;

  // Apply color keying
  if IsColorKeyPresent and not FmtInfo.HasAlphaChannel then
    ApplyColorKey;

  FreeMemNil(Alphas);
end;

{ TNGFileSaver class implementation }

procedure TNGFileSaver.StoreImageToPNGFrame(const IHDR: TIHDR; Bits: Pointer;
  FmtInfo: TImageFormatInfo; IDATStream: TMemoryStream);
var
  TotalBuffer, CompBuffer, ZeroLine, PrevLine: Pointer;
  FilterLines: array[0..4] of PByteArray;
  TotalSize, CompSize, I, BytesPerLine, BytesPerPixel: Integer;
  Filter: Byte;
  Adaptive: Boolean;

  procedure FilterScanline(Filter: Byte; BytesPerPixel: LongInt; Line, PrevLine, Target: PByteArray);
  var
    I: LongInt;
  begin
    case Filter of
      0:
        begin
          // No filter
          Move(Line^, Target^, BytesPerLine);
        end;
      1:
        begin
          // Sub filter
          Move(Line^, Target^, BytesPerPixel);
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] - Line[I - BytesPerPixel]) and $FF;
        end;
      2:
        begin
          // Up filter
          for I := 0 to BytesPerLine - 1 do
            Target[I] := (Line[I] - PrevLine[I]) and $FF;
        end;
      3:
        begin
          // Average filter
          for I := 0 to BytesPerPixel - 1 do
            Target[I] := (Line[I] - PrevLine[I] shr 1) and $FF;
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] - (Line[I - BytesPerPixel] + PrevLine[I]) shr 1) and $FF;
        end;
      4:
        begin
          // Paeth filter
          for I := 0 to BytesPerPixel - 1 do
            Target[I] := (Line[I] - PaethPredictor(0, PrevLine[I], 0)) and $FF;
          for I := BytesPerPixel to BytesPerLine - 1 do
            Target[I] := (Line[I] - PaethPredictor(Line[I - BytesPerPixel], PrevLine[I], PrevLine[I - BytesPerPixel])) and $FF;
        end;
    end;
  end;

  procedure AdaptiveFilter(var Filter: Byte; BytesPerPixel: LongInt; Line, PrevLine, Target: PByteArray);
  var
    I, J, BestTest: LongInt;
    Sums: array[0..4] of LongInt;
  begin
    // Compute the output scanline using all five filters,
    // and select the filter that gives the smallest sum of
    // absolute values of outputs
    FillChar(Sums, SizeOf(Sums), 0);
    BestTest := MaxInt;
    for I := 0 to 4 do
    begin
      FilterScanline(I, BytesPerPixel, Line, PrevLine, FilterLines[I]);
      for J := 0 to BytesPerLine - 1 do
        Sums[I] := Sums[I] + Abs(ShortInt(FilterLines[I][J]));
      if Sums[I] < BestTest then
      begin
        Filter := I;
        BestTest := Sums[I];
      end;
    end;
    Move(FilterLines[Filter]^, Target^, BytesPerLine);
  end;
  
begin
  // Select precompression filter and compression level
  Adaptive := False;
  Filter := 0;
  case PreFilter of
    6:
      if not ((IHDR.BitDepth < 8) or (IHDR.ColorType = 3)) then
        Adaptive := True;
    0..4: Filter := PreFilter;
  else
    if IHDR.ColorType in [2, 6] then
      Filter := 4
  end;

  // Prepare data for compression
  CompBuffer := nil;
  FillChar(FilterLines, SizeOf(FilterLines), 0);
  BytesPerPixel := Max(1, FmtInfo.BytesPerPixel);
  BytesPerLine := FmtInfo.GetPixelsSize(FmtInfo.Format, LongInt(IHDR.Width), 1);
  TotalSize := (BytesPerLine + 1) * LongInt(IHDR.Height);
  GetMem(TotalBuffer, TotalSize);
  GetMem(ZeroLine, BytesPerLine);
  FillChar(ZeroLine^, BytesPerLine, 0);
  PrevLine := ZeroLine;

  if Adaptive then
  begin
    for I := 0 to 4 do
      GetMem(FilterLines[I], BytesPerLine);
  end;

  try
    // Process next scanlines
    for I := 0 to IHDR.Height - 1 do
    begin
      // Filter scanline
      if Adaptive then
      begin
        AdaptiveFilter(Filter, BytesPerPixel, @PByteArray(Bits)[I * BytesPerLine],
          PrevLine, @PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1]);
      end
      else
      begin
        FilterScanline(Filter, BytesPerPixel, @PByteArray(Bits)[I * BytesPerLine],
          PrevLine, @PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1]);
      end;
      PrevLine := @PByteArray(Bits)[I * BytesPerLine];
      // Swap red and blue if necessary
      if (IHDR.ColorType in [2, 6]) and not FmtInfo.IsRBSwapped then
      begin
        SwapRGB(@PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1],
          IHDR.Width, IHDR.BitDepth, BytesPerPixel);
      end;
      // Images with 16 bit channels must be swapped because of PNG's big endianess
      if IHDR.BitDepth = 16 then
      begin
        SwapEndianWord(@PByteArray(TotalBuffer)[I * (BytesPerLine + 1) + 1],
          BytesPerLine div SizeOf(Word));
      end;
      // Set filter used for this scanline
      PByteArray(TotalBuffer)[I * (BytesPerLine + 1)] := Filter;
    end;
    // Compress IDAT data
    CompressBuf(TotalBuffer, TotalSize, CompBuffer, CompSize,
      CompressLevel, ZLibStrategy);
    // Write IDAT data to stream
    IDATStream.WriteBuffer(CompBuffer^, CompSize);
  finally
    FreeMem(TotalBuffer);
    FreeMem(CompBuffer);
    FreeMem(ZeroLine);
    if Adaptive then
      for I := 0 to 4 do
        FreeMem(FilterLines[I]);
  end;
end;

{$IFNDEF DONT_LINK_JNG}

procedure TNGFileSaver.StoreImageToJNGFrame(const JHDR: TJHDR;
  const Image: TImageData; IDATStream, JDATStream,
  JDAAStream: TMemoryStream);
var
  ColorImage, AlphaImage: TImageData;
  FmtInfo: TImageFormatInfo;
  AlphaPtr: PByte;
  GrayPtr: PWordRec;
  ColorPtr: PColor32Rec;
  I: LongInt;
  FakeIHDR: TIHDR;

  procedure SaveJpegToStream(Stream: TStream; const Image: TImageData);
  var
    JpegFormat: TCustomIOJpegFileFormat;
    Handle: TImagingHandle;
    DynImages: TDynImageDataArray;
  begin
    JpegFormat := TCustomIOJpegFileFormat.Create;
    JpegFormat.SetCustomIO(StreamIO);
    // Only JDAT stream can be saved progressive
    if Stream = JDATStream then
      JpegFormat.FProgressive := Progressive
    else
      JpegFormat.FProgressive := False;
    JpegFormat.FQuality := Quality;
    SetLength(DynImages, 1);
    DynImages[0] := Image;
    Handle := StreamIO.Open(Pointer(Stream), omCreate);
    try
      JpegFormat.SaveData(Handle, DynImages, 0);
    finally
      StreamIO.Close(Handle);
      SetLength(DynImages, 0);
      JpegFormat.Free;
    end;
  end;

begin
  GetImageFormatInfo(Image.Format, FmtInfo);
  InitImage(ColorImage);
  InitImage(AlphaImage);

  if FmtInfo.HasAlphaChannel then
  begin
    // Create new image for alpha channel and color image without alpha
    CloneImage(Image, ColorImage);
    NewImage(Image.Width, Image.Height, ifGray8, AlphaImage);
    case Image.Format of
      ifA8Gray8:  ConvertImage(ColorImage, ifGray8);
      ifA8R8G8B8: ConvertImage(ColorImage, ifR8G8B8);
    end;

    // Store source image's alpha to separate image
    AlphaPtr := AlphaImage.Bits;
    if Image.Format = ifA8Gray8 then
    begin
      GrayPtr := Image.Bits;
      for I := 0 to Image.Width * Image.Height - 1 do
      begin
        AlphaPtr^ := GrayPtr.High;
        Inc(GrayPtr);
        Inc(AlphaPtr);
      end;
    end
    else
    begin
      ColorPtr := Image.Bits;
      for I := 0 to Image.Width * Image.Height - 1 do
      begin
        AlphaPtr^ := ColorPtr.A;
        Inc(ColorPtr);
        Inc(AlphaPtr);
      end;
    end;

    // Write color image to stream as JPEG
    SaveJpegToStream(JDATStream, ColorImage);

    if LossyAlpha then
    begin
      // Write alpha image to stream as JPEG
      SaveJpegToStream(JDAAStream, AlphaImage);
    end
    else
    begin
      // Alpha channel is PNG compressed
      FakeIHDR.Width := JHDR.Width;
      FakeIHDR.Height := JHDR.Height;
      FakeIHDR.ColorType := 0;
      FakeIHDR.BitDepth := JHDR.AlphaSampleDepth;
      FakeIHDR.Filter := JHDR.AlphaFilter;
      FakeIHDR.Interlacing := JHDR.AlphaInterlacing;

      GetImageFormatInfo(AlphaImage.Format, FmtInfo);
      StoreImageToPNGFrame(FakeIHDR, AlphaImage.Bits, FmtInfo, IDATStream);
    end;

    FreeImage(ColorImage);
    FreeImage(AlphaImage);
  end
  else
  begin
    // Simply write JPEG to stream
    SaveJpegToStream(JDATStream, Image);
  end;
end;

{$ENDIF}

procedure TNGFileSaver.AddFrame(const Image: TImageData; IsJpegFrame: Boolean);
var
  Frame: TFrameInfo;
  FmtInfo: TImageFormatInfo;
  Index: Integer;

  procedure StorePalette;
  var
    Pal: PPalette24;
    Alphas: PByteArray;
    I, PalBytes: LongInt;
    AlphasDiffer: Boolean;
  begin
    // Fill and save RGB part of palette to PLTE chunk
    PalBytes := FmtInfo.PaletteEntries * SizeOf(TColor24Rec);
    GetMem(Pal, PalBytes);
    AlphasDiffer := False;
    for I := 0 to FmtInfo.PaletteEntries - 1 do
    begin
      Pal[I].B := Image.Palette[I].R;
      Pal[I].G := Image.Palette[I].G;
      Pal[I].R := Image.Palette[I].B;
      if Image.Palette[I].A < 255 then
        AlphasDiffer := True;
    end;
    Frame.Palette := Pal;
    Frame.PaletteEntries := FmtInfo.PaletteEntries;
    // Fill and save alpha part (if there are any alphas < 255) of palette to tRNS chunk
    if AlphasDiffer then
    begin
      PalBytes := FmtInfo.PaletteEntries * SizeOf(Byte);
      GetMem(Alphas, PalBytes);
      for I := 0 to FmtInfo.PaletteEntries - 1 do
        Alphas[I] := Image.Palette[I].A;
      Frame.Transparency := Alphas;
      Frame.TransparencySize := PalBytes;
    end;
  end;

  procedure FillFrameControlChunk(const IHDR: TIHDR; var fcTL: TfcTL);
  var
    Delay: Integer;
  begin
    fcTL.SeqNumber := 0; // Decided when writing to file
    fcTL.Width := IHDR.Width;
    fcTL.Height := IHDR.Height;
    fcTL.XOffset := 0;
    fcTL.YOffset := 0;
    fcTL.DelayNumer := 1;
    fcTL.DelayDenom := 3;
    if FileFormat.FMetadata.HasMetaItemForSaving(SMetaFrameDelay, Index) then
    begin
      // Metadata contains frame delay information in milliseconds
      Delay := FileFormat.FMetadata.MetaItemsForSavingMulti[SMetaFrameDelay, Index];
      fcTL.DelayNumer := Delay;
      fcTL.DelayDenom := 1000;
    end;
    fcTL.DisposeOp := DisposeOpNone;
    fcTL.BlendOp := BlendOpSource;
    SwapEndianUInt32(@fcTL, 5);
    fcTL.DelayNumer := SwapEndianWord(fcTL.DelayNumer);
    fcTL.DelayDenom := SwapEndianWord(fcTL.DelayDenom);
  end;

begin
  // Add new frame
  Frame := AddFrameInfo;
  Frame.IsJpegFrame := IsJpegFrame;
  Index := Length(Frames) - 1;

  with Frame do
  begin
    GetImageFormatInfo(Image.Format, FmtInfo);

    if IsJpegFrame then
    begin
{$IFNDEF DONT_LINK_JNG}
      // Fill JNG header
      JHDR.Width := Image.Width;
      JHDR.Height := Image.Height;
      case Image.Format of
        ifGray8:    JHDR.ColorType := 8;
        ifR8G8B8:   JHDR.ColorType := 10;
        ifA8Gray8:  JHDR.ColorType := 12;
        ifA8R8G8B8: JHDR.ColorType := 14;
      end;
      JHDR.SampleDepth := 8; // 8-bit samples and quantization tables
      JHDR.Compression := 8; // Huffman coding
      JHDR.Interlacing := Iff(Progressive, 8, 0);
      JHDR.AlphaSampleDepth := Iff(FmtInfo.HasAlphaChannel, 8, 0);
      JHDR.AlphaCompression := Iff(LossyAlpha, 8, 0);
      JHDR.AlphaFilter := 0;
      JHDR.AlphaInterlacing := 0;

      StoreImageToJNGFrame(JHDR, Image, IDATMemory, JDATMemory, JDAAMemory);

      // Finally swap endian
      SwapEndianUInt32(@JHDR, 2);
{$ENDIF}
    end
    else
    begin
      // Fill PNG header
      IHDR.Width := Image.Width;
      IHDR.Height := Image.Height;
      IHDR.Compression := 0;
      IHDR.Filter := 0;
      IHDR.Interlacing := 0;
      IHDR.BitDepth := FmtInfo.BytesPerPixel * 8;

      // Select appropiate PNG color type and modify bitdepth
      if FmtInfo.HasGrayChannel then
      begin
        IHDR.ColorType := 0;
        if FmtInfo.HasAlphaChannel then
        begin
          IHDR.ColorType := 4;
          IHDR.BitDepth := IHDR.BitDepth div 2;
        end;
      end
      else if FmtInfo.Format = ifBinary then
      begin
        IHDR.ColorType := 0;
        IHDR.BitDepth := 1;
      end
      else if FmtInfo.IsIndexed then
        IHDR.ColorType := 3
      else if FmtInfo.HasAlphaChannel then
      begin
        IHDR.ColorType := 6;
        IHDR.BitDepth := IHDR.BitDepth div 4;
      end
      else
      begin
        IHDR.ColorType := 2;
        IHDR.BitDepth := IHDR.BitDepth div 3;
      end;

      if FileType = ngAPNG then
      begin
        // Fill fcTL chunk of APNG file
        FillFrameControlChunk(IHDR, fcTL);
      end;

      // Compress PNG image and store it to stream
      StoreImageToPNGFrame(IHDR, Image.Bits, FmtInfo, IDATMemory);
      // Store palette if necesary
      if FmtInfo.IsIndexed then
        StorePalette;

      // Finally swap endian
      SwapEndianUInt32(@IHDR, 2);
    end;
  end;
end;

function TNGFileSaver.SaveFile(Handle: TImagingHandle): Boolean;
var
  I: LongInt;
  Chunk: TChunkHeader;
  SeqNo: UInt32;

  function GetNextSeqNo: UInt32;
  begin
    // Seq numbers of fcTL and fdAT are "interleaved" as they share the counter.
    // Example: first fcTL for IDAT has seq=0, next is fcTL for seond frame with
    // seq=1, then first fdAT with seq=2, fcTL seq=3, fdAT=4, ...
    Result := SwapEndianUInt32(SeqNo);
    Inc(SeqNo);
  end;

  function CalcChunkCrc(const ChunkHdr: TChunkHeader; Data: Pointer;
    Size: LongInt): UInt32;
  begin
    Result := $FFFFFFFF;
    CalcCrc32(Result, @ChunkHdr.ChunkID, SizeOf(ChunkHdr.ChunkID));
    CalcCrc32(Result, Data, Size);
    Result := SwapEndianUInt32(Result xor $FFFFFFFF);
  end;

  procedure WriteChunk(var Chunk: TChunkHeader; ChunkData: Pointer);
  var
    ChunkCrc: UInt32;
    SizeToWrite: LongInt;
  begin
    SizeToWrite := Chunk.DataSize;
    Chunk.DataSize := SwapEndianUInt32(Chunk.DataSize);
    ChunkCrc := CalcChunkCrc(Chunk, ChunkData, SizeToWrite);
    GetIO.Write(Handle, @Chunk, SizeOf(Chunk));
    if SizeToWrite <> 0 then
      GetIO.Write(Handle, ChunkData, SizeToWrite);
    GetIO.Write(Handle, @ChunkCrc, SizeOf(ChunkCrc));
  end;

  procedure WritefdAT(Frame: TFrameInfo);
  var
    ChunkCrc: UInt32;
    ChunkSeqNo: UInt32;
  begin
    Chunk.ChunkID := fdATChunk;
    ChunkSeqNo := GetNextSeqNo;
    // fdAT saves seq number UInt32 before compressed pixels
    Chunk.DataSize := Frame.IDATMemory.Size + SizeOf(UInt32);
    Chunk.DataSize := SwapEndianUInt32(Chunk.DataSize);
    // Calc CRC
    ChunkCrc := $FFFFFFFF;
    CalcCrc32(ChunkCrc, @Chunk.ChunkID, SizeOf(Chunk.ChunkID));
    CalcCrc32(ChunkCrc, @ChunkSeqNo, SizeOf(ChunkSeqNo));
    CalcCrc32(ChunkCrc, Frame.IDATMemory.Memory, Frame.IDATMemory.Size);
    ChunkCrc := SwapEndianUInt32(ChunkCrc xor $FFFFFFFF);
    // Write out all fdAT data
    GetIO.Write(Handle, @Chunk, SizeOf(Chunk));
    GetIO.Write(Handle, @ChunkSeqNo, SizeOf(ChunkSeqNo));
    GetIO.Write(Handle, Frame.IDATMemory.Memory, Frame.IDATMemory.Size);
    GetIO.Write(Handle, @ChunkCrc, SizeOf(ChunkCrc));
  end;

  procedure WriteGlobalMetaDataChunks(Frame: TFrameInfo);
  var
    XRes, YRes: Double;
  begin
    if FileFormat.FMetadata.GetPhysicalPixelSize(ruDpm, XRes, YRes, True) then
    begin
      // Save pHYs chunk
      Frame.pHYs.UnitSpecifier := 1;
      // PNG stores physical resolution as dots per meter
      Frame.pHYs.PixelsPerUnitX := Round(XRes);
      Frame.pHYs.PixelsPerUnitY := Round(YRes);

      Chunk.DataSize := SizeOf(Frame.pHYs);
      Chunk.ChunkID := pHYsChunk;
      SwapEndianUInt32(@Frame.pHYs, SizeOf(Frame.pHYs) div SizeOf(UInt32));
      WriteChunk(Chunk, @Frame.pHYs);
    end;
  end;

  procedure WritePNGMainImageChunks(Frame: TFrameInfo);
  begin
    with Frame do
    begin
      // Write IHDR chunk
      Chunk.DataSize := SizeOf(IHDR);
      Chunk.ChunkID := IHDRChunk;
      WriteChunk(Chunk, @IHDR);
      // Write PLTE chunk if data is present
      if Palette <> nil then
      begin
        Chunk.DataSize := PaletteEntries * SizeOf(TColor24Rec);
        Chunk.ChunkID := PLTEChunk;
        WriteChunk(Chunk, Palette);
      end;
      // Write tRNS chunk if data is present
      if Transparency <> nil then
      begin
        Chunk.DataSize := TransparencySize;
        Chunk.ChunkID := tRNSChunk;
        WriteChunk(Chunk, Transparency);
      end;
    end;
    // Write metadata related chunks
    WriteGlobalMetaDataChunks(Frame);
  end;

begin
  Result := False;
  SeqNo := 0;

  case FileType of
    ngPNG, ngAPNG: GetIO.Write(Handle, @PNGSignature, SizeOf(TChar8));
    ngMNG: GetIO.Write(Handle, @MNGSignature, SizeOf(TChar8));
    ngJNG: GetIO.Write(Handle, @JNGSignature, SizeOf(TChar8));
  end;

  if FileType = ngMNG then
  begin
    // MNG - main header before frames
    SwapEndianUInt32(@MHDR, SizeOf(MHDR) div SizeOf(UInt32));
    Chunk.DataSize := SizeOf(MHDR);
    Chunk.ChunkID := MHDRChunk;
    WriteChunk(Chunk, @MHDR);
  end
  else if FileType = ngAPNG then
  begin
    // APNG - IHDR and global chunks for all frames, then acTL chunk, then frames
    // (fcTL+IDAT, fcTL+fdAT, fcTL+fdAT, fcTL+fdAT, ....)
    WritePNGMainImageChunks(Frames[0]);

    // Animation control chunk
    acTL.NumFrames := Length(Frames);
    if FileFormat.FMetadata.HasMetaItemForSaving(SMetaAnimationLoops) then
    begin
      // Number of plays of APNG animation
      acTL.NumPlay:= FileFormat.FMetadata.MetaItemsForSaving[SMetaAnimationLoops];
    end
    else
      acTL.NumPlay := 0;
    SwapEndianUInt32(@acTL, SizeOf(acTL) div SizeOf(UInt32));

    Chunk.DataSize := SizeOf(acTL);
    Chunk.ChunkID := acTLChunk;
    WriteChunk(Chunk, @acTL);
  end;

  for I := 0 to Length(Frames) - 1 do
  with Frames[I] do
  begin
    if IsJpegFrame then
    begin
      // Write JHDR chunk
      Chunk.DataSize := SizeOf(JHDR);
      Chunk.ChunkID := JHDRChunk;
      WriteChunk(Chunk, @JHDR);
      // Write metadata related chunks
      WriteGlobalMetaDataChunks(Frames[I]);
      // Write JNG image data
      Chunk.DataSize := JDATMemory.Size;
      Chunk.ChunkID := JDATChunk;
      WriteChunk(Chunk, JDATMemory.Memory);
      // Write alpha channel if present
      if JHDR.AlphaSampleDepth > 0 then
      begin
        if JHDR.AlphaCompression = 0 then
        begin
          // Alpha is PNG compressed
          Chunk.DataSize := IDATMemory.Size;
          Chunk.ChunkID := IDATChunk;
          WriteChunk(Chunk, IDATMemory.Memory);
        end
        else
        begin
          // Alpha is JNG compressed
          Chunk.DataSize := JDAAMemory.Size;
          Chunk.ChunkID := JDAAChunk;
          WriteChunk(Chunk, JDAAMemory.Memory);
        end;
      end;
      // Write image end
      Chunk.DataSize := 0;
      Chunk.ChunkID := IENDChunk;
      WriteChunk(Chunk, nil);
    end
    else if FileType <> ngAPNG then
    begin
      // Regular PNG frame (single PNG image or MNG frame)
      WritePNGMainImageChunks(Frames[I]);
      // Write PNG image data
      Chunk.DataSize := IDATMemory.Size;
      Chunk.ChunkID := IDATChunk;
      WriteChunk(Chunk, IDATMemory.Memory);
      // Write image end
      Chunk.DataSize := 0;
      Chunk.ChunkID := IENDChunk;
      WriteChunk(Chunk, nil);
    end
    else if FileType = ngAPNG then
    begin
      // APNG frame - Write fcTL before frame data
      Chunk.DataSize := SizeOf(fcTL);
      Chunk.ChunkID := fcTLChunk;
      fcTl.SeqNumber := GetNextSeqNo;
      WriteChunk(Chunk, @fcTL);
      // Write data - IDAT for first frame and fdAT for following ones
      if I = 0 then
      begin
        Chunk.DataSize := IDATMemory.Size;
        Chunk.ChunkID := IDATChunk;
        WriteChunk(Chunk, IDATMemory.Memory);
      end
      else
        WritefdAT(Frames[I]);
      // Write image end after last frame
      if I = Length(Frames) - 1 then
      begin
        Chunk.DataSize := 0;
        Chunk.ChunkID := IENDChunk;
        WriteChunk(Chunk, nil);
      end;
    end;
  end;

  if FileType = ngMNG then
  begin
    Chunk.DataSize := 0;
    Chunk.ChunkID := MENDChunk;
    WriteChunk(Chunk, nil);
  end;
end;

procedure TNGFileSaver.SetFileOptions;
begin
  PreFilter := FileFormat.FPreFilter;
  CompressLevel := FileFormat.FCompressLevel;
  LossyAlpha := FileFormat.FLossyAlpha;
  Quality := FileFormat.FQuality;
  Progressive := FileFormat.FProgressive;
  ZLibStrategy := FileFormat.FZLibStrategy;
end;

{ TAPNGAnimator class implementation }

class procedure TAPNGAnimator.Animate(var Images: TDynImageDataArray;
  const acTL: TacTL; const SrcFrames: array of TFrameInfo);
var
  I, SrcIdx, Offset, Len: Integer;
  DestFrames: TDynImageDataArray;
  SrcCanvas, DestCanvas: TImagingCanvas;
  PreviousCache: TImageData;
  DestFormat: TImageFormat;
  FormatInfo: TImageFormatInfo;
  AnimatingNeeded, BlendingNeeded: Boolean;

  procedure CheckFrames;
  var
    I: Integer;
  begin
    for I := 0 to Len - 1 do
    with SrcFrames[I] do
    begin
      if (FrameWidth <> Integer(IHDR.Width)) or (FrameHeight <> Integer(IHDR.Height)) or (Len <> Integer(acTL.NumFrames)) or
        (not ((fcTL.DisposeOp = DisposeOpNone) and (fcTL.BlendOp = BlendOpSource)) and
        not ((fcTL.DisposeOp = DisposeOpBackground) and (fcTL.BlendOp = BlendOpSource)) and
        not ((fcTL.DisposeOp = DisposeOpBackground) and (fcTL.BlendOp = BlendOpOver))) then
      begin
        AnimatingNeeded := True;
      end;

      if fcTL.BlendOp = BlendOpOver then
        BlendingNeeded := True;

      if AnimatingNeeded and BlendingNeeded then
        Exit;
    end;
  end;

begin
  AnimatingNeeded := False;
  BlendingNeeded := False;
  Len := Length(SrcFrames);

  CheckFrames;

  if (Len = 0) or not AnimatingNeeded then
    Exit;

  if (Len = Integer(acTL.NumFrames) + 1) and (SrcFrames[0].fcTL.Width = 0) then
  begin
    // If default image (stored in IDAT chunk) isn't part of animation we ignore it
    Offset := 1;
    Len := Len - 1;
  end
  else
    Offset := 0;

  DestFormat := Images[0].Format;
  GetImageFormatInfo(DestFormat, FormatInfo);
  if BlendingNeeded and FormatInfo.IsIndexed then // alpha blending needed -> destination cannot be indexed
    DestFormat := ifA8R8G8B8;

  SetLength(DestFrames, Len);
  DestCanvas := ImagingCanvases.FindBestCanvasForImage(DestFormat).Create;
  SrcCanvas := ImagingCanvases.FindBestCanvasForImage(Images[0]).Create;
  InitImage(PreviousCache);
  NewImage(SrcFrames[0].IHDR.Width, SrcFrames[0].IHDR.Height, DestFormat, PreviousCache);

  for I := 0 to Len - 1 do
  begin
    SrcIdx := I + Offset;

    NewImage(SrcFrames[SrcIdx].IHDR.Width, SrcFrames[SrcIdx].IHDR.Height,
      DestFormat, DestFrames[I]);
    if DestFrames[I].Format = ifIndex8 then
      Move(Images[SrcIdx].Palette^, DestFrames[I].Palette^, 256 * SizeOf(TColor32));

    DestCanvas.CreateForData(@DestFrames[I]);

    if (SrcFrames[SrcIdx].fcTL.DisposeOp = DisposeOpPrevious) and (SrcFrames[SrcIdx - 1].fcTL.DisposeOp <> DisposeOpPrevious) then
    begin
      // Cache current output buffer so we may return to it later (previous dispose op)
      CopyRect(DestFrames[I - 1], 0, 0, DestFrames[I - 1].Width, DestFrames[I - 1].Height,
        PreviousCache, 0, 0);
    end;

    if (I = 0) or (SrcIdx = 0) then
    begin
      // Clear whole frame with transparent black color (default for first frame)
      DestCanvas.FillColor32 := pcClear;
      DestCanvas.Clear;
    end
    else if SrcFrames[SrcIdx - 1].fcTL.DisposeOp = DisposeOpBackground then
    begin
      // Restore background color (clear) on previous frame's area and leave previous content outside of it
      CopyRect(DestFrames[I - 1], 0, 0, DestFrames[I - 1].Width, DestFrames[I - 1].Height,
        DestFrames[I], 0, 0);
      DestCanvas.FillColor32 := pcClear;
      DestCanvas.FillRect(BoundsToRect(SrcFrames[SrcIdx - 1].fcTL.XOffset, SrcFrames[SrcIdx - 1].fcTL.YOffset,
        SrcFrames[SrcIdx - 1].FrameWidth, SrcFrames[SrcIdx - 1].FrameHeight));
    end
    else if SrcFrames[SrcIdx - 1].fcTL.DisposeOp = DisposeOpNone then
    begin
      // Clone previous frame - no change to output buffer
      CopyRect(DestFrames[I - 1], 0, 0, DestFrames[I - 1].Width, DestFrames[I - 1].Height,
        DestFrames[I], 0, 0);
    end
    else if SrcFrames[SrcIdx - 1].fcTL.DisposeOp = DisposeOpPrevious then
    begin
      // Revert to previous frame (cached, can't just restore DestFrames[I - 2])
      CopyRect(PreviousCache, 0, 0, PreviousCache.Width, PreviousCache.Height,
        DestFrames[I], 0, 0);
    end;

    // Copy pixels or alpha blend them over
    if SrcFrames[SrcIdx].fcTL.BlendOp = BlendOpSource then
    begin
      CopyRect(Images[SrcIdx], 0, 0, Images[SrcIdx].Width, Images[SrcIdx].Height,
        DestFrames[I], SrcFrames[SrcIdx].fcTL.XOffset, SrcFrames[SrcIdx].fcTL.YOffset);
    end
    else if SrcFrames[SrcIdx].fcTL.BlendOp = BlendOpOver then
    begin
      SrcCanvas.CreateForData(@Images[SrcIdx]);
      SrcCanvas.DrawAlpha(SrcCanvas.ClipRect, DestCanvas,
        SrcFrames[SrcIdx].fcTL.XOffset, SrcFrames[SrcIdx].fcTL.YOffset);
    end;

    FreeImage(Images[SrcIdx]);
  end;

  DestCanvas.Free;
  SrcCanvas.Free;
  FreeImage(PreviousCache);

  // Assign dest frames to final output images
  Images := DestFrames;
end;

{ TNetworkGraphicsFileFormat class implementation }

procedure TNetworkGraphicsFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffSave];

  FPreFilter := NGDefaultPreFilter;
  FCompressLevel := NGDefaultCompressLevel;
  FLossyAlpha := NGDefaultLossyAlpha;
  FLossyCompression := NGDefaultLossyCompression;
  FQuality := NGDefaultQuality;
  FProgressive := NGDefaultProgressive;
  FZLibStrategy := NGDefaultZLibStrategy;
end;

procedure TNetworkGraphicsFileFormat.CheckOptionsValidity;
begin
  // Just check if save options has valid values
  if not (FPreFilter in [0..6]) then
    FPreFilter := NGDefaultPreFilter;
  if not (FCompressLevel in [0..9]) then
    FCompressLevel := NGDefaultCompressLevel;
  if not (FQuality in [1..100]) then
    FQuality := NGDefaultQuality;
end;

function TNetworkGraphicsFileFormat.GetSupportedFormats: TImageFormats;
begin
  if FLossyCompression then
    Result := NGLossyFormats
  else
    Result := NGLosslessFormats;
end;

procedure TNetworkGraphicsFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if not FLossyCompression then
  begin
    // Convert formats for lossless compression
    if Info.HasGrayChannel then
    begin
      if Info.HasAlphaChannel then
      begin
        if Info.BytesPerPixel <= 2 then
          // Convert <= 16bit grayscale images with alpha to ifA8Gray8
          ConvFormat := ifA8Gray8
        else
          // Convert > 16bit grayscale images with alpha to ifA16Gray16
          ConvFormat := ifA16Gray16
      end
      else
        // Convert grayscale images without alpha to ifGray16
        ConvFormat := ifGray16;
    end
    else
      if Info.IsFloatingPoint then
        // Convert floating point images to 64 bit ARGB (or RGB if no alpha)
        ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16B16G16R16, ifB16G16R16)
      else if Info.HasAlphaChannel or Info.IsSpecial then
        // Convert all other images with alpha or special images to A8R8G8B8
        ConvFormat := ifA8R8G8B8
      else
        // Convert images without alpha to R8G8B8
        ConvFormat := ifR8G8B8;
  end
  else
  begin
    // Convert formats for lossy compression
    if Info.HasGrayChannel then
      ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8Gray8, ifGray8)
    else
      ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);
  end;

  ConvertImage(Image, ConvFormat);
end;

function TNetworkGraphicsFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  Sig: TChar8;
begin
  Result := False;
  if Handle <> nil then
    with GetIO do
    begin
      FillChar(Sig, SizeOf(Sig), 0);
      ReadCount := Read(Handle, @Sig, SizeOf(Sig));
      Seek(Handle, -ReadCount, smFromCurrent);
      Result := (ReadCount = SizeOf(Sig)) and (Sig = FSignature);
    end;
end;

{ TPNGFileFormat class implementation }

procedure TPNGFileFormat.Define;
begin
  inherited;
  FName := SPNGFormatName;
  FFeatures := FFeatures + [ffMultiImage];
  FLoadAnimated := PNGDefaultLoadAnimated;
  AddMasks(SPNGMasks);

  FSignature := PNGSignature;

  RegisterOption(ImagingPNGPreFilter, @FPreFilter);
  RegisterOption(ImagingPNGCompressLevel, @FCompressLevel);
  RegisterOption(ImagingPNGLoadAnimated, @FLoadAnimated);
  RegisterOption(ImagingPNGZLibStrategy, @FZLibStrategy);
end;

function TPNGFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  I, Len: LongInt;
  NGFileLoader: TNGFileLoader;
begin
  Result := False;
  NGFileLoader := TNGFileLoader.Create(Self);
  try
    // Use NG file parser to load file
    if NGFileLoader.LoadFile(Handle) and (Length(NGFileLoader.Frames) > 0) then
    begin
      Len := Length(NGFileLoader.Frames);
      SetLength(Images, Len);
      for I := 0 to Len - 1 do
      with NGFileLoader.Frames[I] do
      begin
        // Build actual image bits
        if not IsJpegFrame then
          NGFileLoader.LoadImageFromPNGFrame(FrameWidth, FrameHeight, IHDR, IDATMemory, Images[I]);
        // Build palette, aply color key or background

        NGFileLoader.ApplyFrameSettings(NGFileLoader.Frames[I], Images[I]);
        Result := True;
      end;
      // Animate APNG images
      if (NGFileLoader.FileType = ngAPNG) and FLoadAnimated then
        TAPNGAnimator.Animate(Images, NGFileLoader.acTL, NGFileLoader.Frames);
    end;
  finally
    NGFileLoader.LoadMetaData; // Store metadata
    NGFileLoader.Free;
  end;
end;

function TPNGFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  I: Integer;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  NGFileSaver: TNGFileSaver;
  DefaultFormat: TImageFormat;
  Screen: TImageData;
  AnimWidth, AnimHeight: Integer;
begin
  Result := False;
  DefaultFormat := ifDefault;
  AnimWidth := 0;
  AnimHeight := 0;
  NGFileSaver := TNGFileSaver.Create(Self);

  // Save images with more frames as APNG format
  if Length(Images) > 1 then
  begin
    NGFileSaver.FileType := ngAPNG;
    // Get max dimensions of frames
    AnimWidth := Images[FFirstIdx].Width;
    AnimHeight := Images[FFirstIdx].Height;
    for I := FFirstIdx + 1 to FLastIdx do
    begin
      AnimWidth := Max(AnimWidth, Images[I].Width);
      AnimHeight := Max(AnimHeight, Images[I].Height);
    end;
  end
  else
    NGFileSaver.FileType := ngPNG;

  NGFileSaver.SetFileOptions;

  with NGFileSaver do
  try
    // Store all frames to be saved frames file saver
    for I := FFirstIdx to FLastIdx do
    begin
      if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
      try
        if FileType = ngAPNG then
        begin
          // IHDR chunk is shared for all frames so all frames must have the
          // same data format as the first image.
          if I = FFirstIdx then
          begin
            DefaultFormat := ImageToSave.Format;
            // Subsequenet frames may be bigger than the first one.
            // APNG doens't support this - max allowed size is what's written in
            // IHDR - size of main/default/first image. If some frame is
            // bigger than the first one we need to resize (create empty bigger
            // image and copy) the first frame so all following frames could fit to
            // its area.
            if (ImageToSave.Width <> AnimWidth) or (ImageToSave.Height <> AnimHeight) then
            begin
              InitImage(Screen);
              NewImage(AnimWidth, AnimHeight, ImageToSave.Format, Screen);
              CopyRect(ImageToSave, 0, 0, ImageToSave.Width, ImageToSave.Height, Screen, 0, 0);
              if MustBeFreed then
                FreeImage(ImageToSave);
              ImageToSave := Screen;
            end;
          end
          else if ImageToSave.Format <> DefaultFormat then
          begin
            if MustBeFreed then
              ConvertImage(ImageToSave, DefaultFormat)
            else
            begin
              CloneImage(Images[I], ImageToSave);
              ConvertImage(ImageToSave, DefaultFormat);
              MustBeFreed := True;
            end;
          end;
        end;

        // Add image as PNG frame
        AddFrame(ImageToSave, False);
      finally
        if MustBeFreed then
          FreeImage(ImageToSave);
      end
      else
        Exit;
    end;

    // Finally save PNG file
    SaveFile(Handle);
    Result := True;
  finally
    NGFileSaver.Free;
  end;
end;

{$IFNDEF DONT_LINK_MNG}

{ TMNGFileFormat class implementation }

procedure TMNGFileFormat.Define;
begin
  inherited;
  FName := SMNGFormatName;
  FFeatures := FFeatures + [ffMultiImage];
  AddMasks(SMNGMasks);

  FSignature := MNGSignature;

  RegisterOption(ImagingMNGLossyCompression, @FLossyCompression);
  RegisterOption(ImagingMNGLossyAlpha, @FLossyAlpha);
  RegisterOption(ImagingMNGPreFilter, @FPreFilter);
  RegisterOption(ImagingMNGCompressLevel, @FCompressLevel);
  RegisterOption(ImagingMNGQuality, @FQuality);
  RegisterOption(ImagingMNGProgressive, @FProgressive);
end;

function TMNGFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  NGFileLoader: TNGFileLoader;
  I, Len: LongInt;
begin
  Result := False;
  NGFileLoader := TNGFileLoader.Create(Self);
  try
    // Use NG file parser to load file
    if NGFileLoader.LoadFile(Handle) then
    begin
      Len := Length(NGFileLoader.Frames);
      if Len > 0 then
      begin
        SetLength(Images, Len);
        for I := 0 to Len - 1 do
        with NGFileLoader.Frames[I] do
        begin
          // Build actual image bits
          if IsJpegFrame then
            NGFileLoader.LoadImageFromJNGFrame(FrameWidth, FrameHeight, JHDR, IDATMemory, JDATMemory, JDAAMemory, Images[I])
          else
            NGFileLoader.LoadImageFromPNGFrame(FrameWidth, FrameHeight, IHDR, IDATMemory, Images[I]);
          // Build palette, aply color key or background
          NGFileLoader.ApplyFrameSettings(NGFileLoader.Frames[I], Images[I]);
        end;
      end
      else
      begin
        // Some MNG files (with BASI-IEND streams) dont have actual pixel data
        SetLength(Images, 1);
        NewImage(NGFileLoader.MHDR.FrameWidth, NGFileLoader.MHDR.FrameWidth, ifDefault, Images[0]);
      end;
      Result := True;
    end;
  finally
    NGFileLoader.LoadMetaData; // Store metadata
    NGFileLoader.Free;
  end;
end;

function TMNGFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  NGFileSaver: TNGFileSaver;
  I, LargestWidth, LargestHeight: LongInt;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
begin
  Result := False;
  LargestWidth := 0;
  LargestHeight := 0;

  NGFileSaver := TNGFileSaver.Create(Self);
  NGFileSaver.FileType := ngMNG;
  NGFileSaver.SetFileOptions;

  with NGFileSaver do
  try
    // Store all frames to be saved frames file saver
    for I := FFirstIdx to FLastIdx do
    begin
      if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
      try
        // Add image as PNG or JNG frame
        AddFrame(ImageToSave, FLossyCompression);
        // Remember largest frame width and height
        LargestWidth := Iff(LargestWidth < ImageToSave.Width, ImageToSave.Width, LargestWidth);
        LargestHeight := Iff(LargestHeight < ImageToSave.Height, ImageToSave.Height, LargestHeight);
      finally
        if MustBeFreed then
          FreeImage(ImageToSave);
      end
      else
        Exit;
    end;

    // Fill MNG header
    MHDR.FrameWidth := LargestWidth;
    MHDR.FrameHeight := LargestHeight;
    MHDR.TicksPerSecond := 0;
    MHDR.NominalLayerCount := 0;
    MHDR.NominalFrameCount := Length(Frames);
    MHDR.NominalPlayTime := 0;
    MHDR.SimplicityProfile := 473; // 111011001 binary, defines MNG-VLC with transparency and JNG support

    // Finally save MNG file
    SaveFile(Handle);
    Result := True;
  finally
    NGFileSaver.Free;
  end;
end;

{$ENDIF}

{$IFNDEF DONT_LINK_JNG}

{ TJNGFileFormat class implementation }

procedure TJNGFileFormat.Define;
begin
  inherited;
  FName := SJNGFormatName;
  AddMasks(SJNGMasks);

  FSignature := JNGSignature;
  FLossyCompression := True;

  RegisterOption(ImagingJNGLossyAlpha, @FLossyAlpha);
  RegisterOption(ImagingJNGAlphaPreFilter, @FPreFilter);
  RegisterOption(ImagingJNGAlphaCompressLevel, @FCompressLevel);
  RegisterOption(ImagingJNGQuality, @FQuality);
  RegisterOption(ImagingJNGProgressive, @FProgressive);

end;

function TJNGFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  NGFileLoader: TNGFileLoader;
begin
  Result := False;
  NGFileLoader := TNGFileLoader.Create(Self);
  try
    // Use NG file parser to load file
    if NGFileLoader.LoadFile(Handle) and (Length(NGFileLoader.Frames) > 0) then
    with NGFileLoader.Frames[0] do
    begin
      SetLength(Images, 1);
      // Build actual image bits
      if IsJpegFrame then
        NGFileLoader.LoadImageFromJNGFrame(FrameWidth, FrameHeight, JHDR, IDATMemory, JDATMemory, JDAAMemory, Images[0]);
      // Build palette, aply color key or background
      NGFileLoader.ApplyFrameSettings(NGFileLoader.Frames[0], Images[0]);
      Result := True;
    end;
  finally
    NGFileLoader.LoadMetaData; // Store metadata
    NGFileLoader.Free;
  end;
end;

function TJNGFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  NGFileSaver: TNGFileSaver;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
begin
  // Make image JNG compatible, store it in saver, and save it to file
  Result := MakeCompatible(Images[Index], ImageToSave, MustBeFreed);
  if Result then
  begin
    NGFileSaver := TNGFileSaver.Create(Self);
    with NGFileSaver do
    try
      FileType := ngJNG;
      SetFileOptions;
      AddFrame(ImageToSave, True);
      SaveFile(Handle);
    finally
      // Free NG saver and compatible image
      NGFileSaver.Free;
      if MustBeFreed then
        FreeImage(ImageToSave);
    end;
  end;
end;

{$ENDIF}

initialization
  RegisterImageFileFormat(TPNGFileFormat);
{$IFNDEF DONT_LINK_MNG}
  RegisterImageFileFormat(TMNGFileFormat);
{$ENDIF}
{$IFNDEF DONT_LINK_JNG}
  RegisterImageFileFormat(TJNGFileFormat);
{$ENDIF}
finalization

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77 Changes/Bug Fixes -----------------------------------
    - Reads and writes APNG animation loop count metadata.
    - Writes frame delays of APNG from metadata.
    - Fixed color keys in 8bit depth PNG/MNG loading.
    - Fixed needless (and sometimes buggy) conversion to format with alpha
      channel in FPC (GetMem(0) <> nil!).
    - Added support for optional ZLib compression strategy.
    - Added loading and saving of ifBinary (1bit black and white)
      format images. During loading grayscale 1bpp and indexed 1bpp
      (with only black and white colors in palette) are treated as ifBinary.
      ifBinary are saved as 1bpp grayscale PNGs.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Reads frame delays from APNG files into metadata.
    - Added loading and saving of metadata from these chunks: pHYs.
    - Simplified decoding of 1/2/4 bit images a bit (less code).

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Added APNG saving support.
    - Added APNG support to NG loader and animating to PNG loader.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Changed file format conditional compilation to reflect changes
      in LINK symbols.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Changes for better thread safety.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added loading of global palettes and transparencies in MNG files
      (and by doing so fixed crash when loading images with global PLTE or tRNS).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Small changes in converting to supported formats.
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Made public properties for options registered to SetOption/GetOption
      functions.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - MNG and JNG support added, PNG support redesigned to support NG file handlers
    - added classes for working with NG file formats
    - stuff from old ImagingPng unit added and that unit was deleted
    - unit created and initial stuff added
    
  -- 0.15 Changes/Bug Fixes -----------------------------------
    - when saving indexed images save alpha to tRNS?
    - added some defines and ifdefs to dzlib unit to allow choosing
      impaszlib, fpc's paszlib, zlibex or other zlib implementation
    - added colorkeying support
    - fixed 16bit channel image handling - pixels were not swapped
    - fixed arithmetic overflow (in paeth filter) in FPC
    - data of unknown chunks are skipped and not needlesly loaded

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - adaptive filtering added to PNG saving
    - TPNGFileFormat class added
}

end.
