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

{ This unit contains image format loader/saver for TIFF images
  using LibTiff C library compiled to object files or LibTiff DLL/SO.

  Supported platforms/compilers are now:
    Win32 Delphi: obj, dll
    Win64 Delphi: dll
    Win32, Win64 FPC: obj, dll
    Linux/Unix/macOS 32/64 FPC: dll
}
unit ImagingTiffLib;

{$I ImagingOptions.inc}

{$IF Defined(LINUX) or Defined(BSD) or Defined(MACOS)}
  // Use LibTiff dynamic library in Linux/BSD instead of precompiled objects.
  // It's installed on most systems so let's use it and keep the binary smaller.
  // In macOS it's usually not installed but if it is let's use it.
  {$DEFINE USE_DYN_LIB}
{$IFEND}

{$IF Defined(DCC) and Defined(WIN64)}
  // For Delphi Win64 target try to use LibTiff dynamic library.
  {$DEFINE USE_DYN_LIB}
{$IFEND}

{$IF Defined(POSIX) and Defined(CPUX64)}
  // Workaround for problem on 64bit Linux where thandle_t in libtiff is
  // still 32bit so it cannot be used to pass pointers (for IO functions).
  {$DEFINE HANDLE_NOT_POINTER_SIZED}
{$IFEND}

{.$DEFINE USE_DYN_LIB}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility, ImagingIO,
  ImagingTiff,
{$IFDEF USE_DYN_LIB}
  LibTiffDynLib;
{$ELSE}
  LibTiffDelphi;
{$ENDIF}

type
  { TIFF (Tag Image File Format) loader/saver class. Uses LibTiff so
    it can handle most types of TIFF files.}
  TTiffLibFileFormat = class(TBaseTiffFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: Integer): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

implementation

const
  TiffSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA8Gray8,
    ifGray16, ifA16Gray16, ifGray32, ifR8G8B8, ifA8R8G8B8, ifR16G16B16,
    ifA16R16G16B16, ifR32F, ifA32R32G32B32F, ifR16F, ifA16R16G16B16F, ifBinary];

type
  TTiffIOWrapper = record
    IO: TIOFunctions;
    Handle: TImagingHandle;
  end;
  PTiffIOWrapper = ^TTiffIOWrapper;

{$IFDEF HANDLE_NOT_POINTER_SIZED}
var
  TiffIOWrapper: TTiffIOWrapper;
{$ENDIF}

function GetTiffIOWrapper(Fd: THandle): PTiffIOWrapper;
begin
{$IFDEF HANDLE_NOT_POINTER_SIZED}
  Result := @TiffIOWrapper;
{$ELSE}
  Result := PTiffIOWrapper(Fd);
{$ENDIF}
end;

function TIFFReadProc(Fd: THandle; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := GetTiffIOWrapper(Fd);
  Result := Wrapper.IO.Read(Wrapper.Handle, Buffer, Size);
end;

function TIFFWriteProc(Fd: THandle; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := GetTiffIOWrapper(Fd);
  Result := Wrapper.IO.Write(Wrapper.Handle, Buffer, Size);
end;

function TIFFSizeProc(Fd: THandle): toff_t; cdecl;
var
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := GetTiffIOWrapper(Fd);
  Result := ImagingIO.GetInputSize(Wrapper.IO, Wrapper.Handle);
end;

function TIFFSeekProc(Fd: THandle; Offset: toff_t; Where: Integer): toff_t; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  Mode: TSeekMode;
  Wrapper: PTiffIOWrapper;
begin
  Wrapper := GetTiffIOWrapper(Fd);
  if Offset = $FFFFFFFF then
  begin
    Result := $FFFFFFFF;
    Exit;
  end;
  case Where of
    SEEK_SET: Mode := smFromBeginning;
    SEEK_CUR: Mode := smFromCurrent;
    SEEK_END: Mode := smFromEnd;
  else
    Mode := smFromBeginning;
  end;
  Result := Wrapper.IO.Seek(Wrapper.Handle, Offset, Mode);
end;

function TIFFCloseProc(Fd: THandle): Integer; cdecl;
begin
  Result := 0;
end;

function TIFFNoMapProc(Fd: THandle; Base: PPointer; Size: PCardinal): Integer; cdecl;
begin
  Result := 0;
end;

procedure TIFFNoUnmapProc(Fd: THandle; Base: Pointer; Size: Cardinal); cdecl;
begin
end;

var
  LastError: string = 'None';

procedure TIFFErrorHandler(const Module, Message: AnsiString);
begin
  LastError := string(Module + ': ' + Message);
end;

{
  TTiffFileFormat implementation
}

procedure TTiffLibFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffSave, ffMultiImage];
  FSupportedFormats := TiffSupportedFormats;
end;

function TTiffLibFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Tiff: PTIFF;
  IOWrapper: TTiffIOWrapper;
  I, Idx, TiffResult, ScanLineSize, NumDirectories, X: Integer;
  RowsPerStrip: UInt32;
  Orientation, BitsPerSample, SamplesPerPixel, Photometric,
    PlanarConfig, SampleFormat: Word;
  DataFormat: TImageFormat;
  CanAccessScanlines: Boolean;
  Ptr: PByte;
  Red, Green, Blue: PWordRecArray;

  procedure LoadMetadata(Tiff: PTiff; PageIndex: Integer);
  var
    TiffResUnit, CompressionScheme: Word;
    XRes, YRes: Single;
    ResUnit: TResolutionUnit;
    CompressionName: string;
    HasResolution: Boolean;
  begin
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_RESOLUTIONUNIT, @TiffResUnit);
    FMetadata.SetMetaItem(SMetaTiffResolutionUnit, TiffResUnit, PageIndex);

    HasResolution := (TIFFGetField(Tiff, TIFFTAG_XRESOLUTION, @XRes) = 1) and
                     (TIFFGetField(Tiff, TIFFTAG_YRESOLUTION, @YRes) = 1);

    if HasResolution and (TiffResUnit <> RESUNIT_NONE) and (XRes >= 0.1) and (YRes >= 0.1) then
    begin
      ResUnit := ruDpi;
      if TiffResUnit = RESUNIT_CENTIMETER then
        ResUnit := ruDpcm;
      FMetadata.SetPhysicalPixelSize(ResUnit, XRes, YRes, False, PageIndex);
    end;

    TIFFGetFieldDefaulted(Tiff, TIFFTAG_COMPRESSION, @CompressionScheme);

    case CompressionScheme of
      COMPRESSION_NONE: CompressionName := 'None';
      COMPRESSION_LZW:  CompressionName := 'LZW';
      COMPRESSION_JPEG: CompressionName := 'JPEG';
      COMPRESSION_PACKBITS:  CompressionName := 'Packbits RLE';
      COMPRESSION_DEFLATE:   CompressionName := 'Deflate';
      COMPRESSION_CCITTFAX4: CompressionName := 'CCITT Group 4 Fax';
      COMPRESSION_OJPEG: CompressionName := 'Old JPEG';
      COMPRESSION_CCITTRLE..COMPRESSION_CCITTFAX3: CompressionName := 'CCITT';
    else
      CompressionName := 'Unknown';
    end;

    FMetadata.SetMetaItem(SMetaTiffCompressionName, CompressionName, PageIndex);
  end;

begin
  Result := False;
  SetUserMessageHandlers(TIFFErrorHandler, nil);

  // Set up IO wrapper and open TIFF
  IOWrapper.IO := GetIO;
  IOWrapper.Handle := Handle;
{$IFDEF HANDLE_NOT_POINTER_SIZED}
  TiffIOWrapper := IOWrapper;
{$ENDIF}

  { Castle Game Engine added }
  { Avoid range check errors, in case this unit is compiled by CGE build tool
    in debug mode.
    Observed with FPC 3.2.2 on Linux/x86_64, open Tiger.tif .

    TODO: The cast to THandle here, and then to 32-bit Cardinal,
    is bad on 64-bit! }

  {$ifdef FPC} {$push} {$R-} {$endif}
  Tiff := TIFFClientOpen('LibTIFF', 'r', THandle(@IOWrapper), @TIFFReadProc,
    @TIFFWriteProc, @TIFFSeekProc, @TIFFCloseProc,
    @TIFFSizeProc, @TIFFNoMapProc, @TIFFNoUnmapProc);
  {$ifdef FPC} {$pop} {$endif}

  if Tiff <> nil then
    TIFFSetFileNo(Tiff, THandle(@IOWrapper))
  else
    Exit;

  NumDirectories := TIFFNumberOfDirectories(Tiff);
  if OnlyFirstLevel then
    NumDirectories := Min(1, NumDirectories);

  SetLength(Images, NumDirectories);

  for Idx := 0 to NumDirectories - 1 do
  begin
    TIFFSetDirectory(Tiff, Idx);

    // Set defaults for TIFF fields
    DataFormat := ifUnknown;

    // Read some TIFF fields with basic image info
    TIFFGetField(Tiff, TIFFTAG_IMAGEWIDTH, @Images[Idx].Width);
    TIFFGetField(Tiff, TIFFTAG_IMAGELENGTH, @Images[Idx].Height);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_ORIENTATION, @Orientation);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_BITSPERSAMPLE, @BitsPerSample);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_SAMPLESPERPIXEL, @SamplesPerPixel);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_SAMPLEFORMAT, @SampleFormat);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_PHOTOMETRIC, @Photometric);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_PLANARCONFIG, @PlanarConfig);
    TIFFGetFieldDefaulted(Tiff, TIFFTAG_ROWSPERSTRIP, @RowsPerStrip);

    // Load supported metadata
    LoadMetadata(Tiff, Idx);
    // See if we can just copy scanlines from TIFF to Imaging image
    CanAccessScanlines := (PlanarConfig = PLANARCONFIG_CONTIG) or (SamplesPerPixel = 1);

    if CanAccessScanlines then
    begin
      // We can copy scanlines so we try to find data format that best matches
      // TIFFs internal data format
      if (Photometric = PHOTOMETRIC_MINISBLACK) or (Photometric = PHOTOMETRIC_MINISWHITE) then
      begin
        if SampleFormat = SAMPLEFORMAT_UINT then
        begin
          case BitsPerSample of
             1:
               if SamplesPerPixel = 1 then
                 DataFormat := ifBinary;
             8:
               case SamplesPerPixel of
                 1: DataFormat := ifGray8;
                 2: DataFormat := ifA8Gray8;
               end;
            16:
               case SamplesPerPixel of
                 1: DataFormat := ifGray16;
                 2: DataFormat := ifA16Gray16;
               end;
            32:
               if SamplesPerPixel = 1 then
                 DataFormat := ifGray32;
          end;
        end
        else if SampleFormat = SAMPLEFORMAT_IEEEFP then
        begin
          case BitsPerSample of
            16:
               if SamplesPerPixel = 1 then
                 DataFormat := ifR16F;
            32:
               if SamplesPerPixel = 1 then
                 DataFormat := ifR32F;
          end;
        end;
      end
      else if Photometric = PHOTOMETRIC_RGB then
      begin
        if SampleFormat = SAMPLEFORMAT_UINT then
        begin
          case BitsPerSample of
             8:
               case SamplesPerPixel of
                 3: DataFormat := ifR8G8B8;
                 4: DataFormat := ifA8R8G8B8;
               end;
            16:
               case SamplesPerPixel of
                 3: DataFormat := ifR16G16B16;
                 4: DataFormat := ifA16R16G16B16;
               end;
          end;
        end
        else if SampleFormat = SAMPLEFORMAT_IEEEFP then
        begin
          case BitsPerSample of
            16:
               if SamplesPerPixel = 4 then
                 DataFormat := ifA16R16G16B16F;
            32:
               if SamplesPerPixel = 4 then
                 DataFormat := ifA32R32G32B32F;
          end;
        end;
      end
      else if Photometric = PHOTOMETRIC_PALETTE then
      begin
        if (SamplesPerPixel = 1) and (SampleFormat = SAMPLEFORMAT_UINT) and (BitsPerSample = 8) then
          DataFormat := ifIndex8
      end;
    end;

    if DataFormat = ifUnknown then
    begin
      // Use RGBA interface to read A8R8G8B8 TIFFs and mainly TIFFs in various
      // formats with no Imaging equivalent, exotic color spaces etc.
      NewImage(Images[Idx].Width, Images[Idx].Height, ifA8R8G8B8, Images[Idx]);
      TiffResult := TIFFReadRGBAImageOriented(Tiff, Images[Idx].Width, Images[Idx].Height,
        Images[Idx].Bits, Orientation, 0);
      if TiffResult = 0 then
        RaiseImaging(LastError, []);
      // Swap Red and Blue, if YCbCr.
      if Photometric = PHOTOMETRIC_YCBCR then
        SwapChannels(Images[Idx], ChannelRed, ChannelBlue);
    end
    else
    begin
      // Create new image in given format and read scanlines from TIFF,
      // read palette too if needed
      NewImage(Images[Idx].Width, Images[Idx].Height, DataFormat, Images[Idx]);
      ScanLineSize := TIFFScanlineSize(Tiff);

      for I := 0 to Images[Idx].Height - 1 do
        TIFFReadScanline(Tiff, @PByteArray(Images[Idx].Bits)[I * ScanLineSize], I, 0);

      if DataFormat = ifIndex8 then
      begin
        TIFFGetField(Tiff, TIFFTAG_COLORMAP, @Red, @Green, @Blue);
        for I := 0 to 255 do
        with Images[Idx].Palette[I] do
        begin
          A := 255;
          R := Red[I].High;
          G := Green[I].High;
          B := Blue[I].High;
        end;
      end;

      // TIFF uses BGR order so we must swap it (but not images we got
      // from TiffLib RGBA interface)
      if Photometric = PHOTOMETRIC_RGB then
        SwapChannels(Images[Idx], ChannelRed, ChannelBlue);

      // We need to negate 'MinIsWhite' formats to get common grayscale
      // formats where min sample value is black
      if Photometric = PHOTOMETRIC_MINISWHITE then
        for I := 0 to Images[Idx].Height - 1 do
        begin
          Ptr := @PByteArray(Images[Idx].Bits)[I * ScanLineSize];
          for X := 0 to ScanLineSize - 1 do
          begin
            Ptr^ := not Ptr^;
            Inc(Ptr);
          end;
        end;
    end;
  end;

  TIFFClose(Tiff);
  Result := True;
end;

function TTiffLibFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
const
  Compressions: array[0..5] of Word = (COMPRESSION_NONE, COMPRESSION_LZW,
    COMPRESSION_PACKBITS, COMPRESSION_DEFLATE, COMPRESSION_JPEG, COMPRESSION_CCITTFAX4);
var
  Tiff: PTIFF;
  IOWrapper: TTiffIOWrapper;
  I, J, ScanLineSize: Integer;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Info: TImageFormatInfo;
  Orientation, BitsPerSample, SamplesPerPixel, Photometric,
    PlanarConfig, SampleFormat, CompressionScheme: Word;
  RowsPerStrip: UInt32;
  Red, Green, Blue: array[Byte] of TWordRec;
  CompressionMismatch: Boolean;
  OpenMode: PAnsiChar;

  procedure SaveMetadata(Tiff: PTiff; PageIndex: Integer);
  var
    XRes, YRes: Double;
    ResUnit: TResolutionUnit;
    TiffResUnit, StoredTiffResUnit: Word;
  begin
    XRes := -1;
    YRes := -1;

    ResUnit := ruDpcm;
    TiffResUnit := RESUNIT_CENTIMETER;

    if FMetadata.HasMetaItemForSaving(SMetaTiffResolutionUnit) then
    begin
      // Check if DPI resolution unit is requested to be used (e.g. to
      // use the same unit when just resaving files)
      StoredTiffResUnit := FMetadata.MetaItemsForSaving[SMetaTiffResolutionUnit];
      if StoredTiffResUnit = RESUNIT_INCH then
      begin
        ResUnit := ruDpi;
        TiffResUnit := RESUNIT_INCH;
      end;
    end;

    // First try to find phys. size for current TIFF page index. If not found then
    // try size for main image (index 0).
    if not FMetadata.GetPhysicalPixelSize(ResUnit, XRes, YRes, True, PageIndex) then
      FMetadata.GetPhysicalPixelSize(ResUnit, XRes, YRes, True, 0);

    if (XRes > 0) and (YRes > 0) then
    begin
      TIFFSetField(Tiff, TIFFTAG_RESOLUTIONUNIT, TiffResUnit);
      // Resolution tags are defined as 32bit float in TIFF docs
      // but libtiff handles double input just fine.
      TIFFSetField(Tiff, TIFFTAG_XRESOLUTION, XRes);
      TIFFSetField(Tiff, TIFFTAG_YRESOLUTION, YRes);
    end;
  end;

begin
  Result := False;
  SetUserMessageHandlers(TIFFErrorHandler, nil);

  if not (FCompression in [0..5]) then
    FCompression := COMPRESSION_LZW;

  // Set up IO wrapper and open TIFF
  IOWrapper.IO := GetIO;
  IOWrapper.Handle := Handle;
{$IFDEF HANDLE_NOT_POINTER_SIZED}
  TiffIOWrapper := IOWrapper;
{$ENDIF}

  OpenMode := 'w';

  Tiff := TIFFClientOpen('LibTIFF', OpenMode, THandle(@IOWrapper), @TIFFReadProc,
    @TIFFWriteProc, @TIFFSeekProc, @TIFFCloseProc,
    @TIFFSizeProc, @TIFFNoMapProc, @TIFFNoUnmapProc);

  if Tiff <> nil then
    TIFFSetFileNo(Tiff, THandle(@IOWrapper))
  else
    Exit;

  for I := FFirstIdx to FLastIdx do
  begin
    if MakeCompatible(Images[I], ImageToSave, MustBeFreed) then
    with GetIO, ImageToSave do
    try
      GetImageFormatInfo(Format, Info);

      // Set Tag values
      Orientation := ORIENTATION_TOPLEFT;
      BitsPerSample := Info.BytesPerPixel div Info.ChannelCount * 8;
      if Info.Format = ifBinary then
        BitsPerSample := 1;
      SamplesPerPixel := Info.ChannelCount;
      SampleFormat := Iff(not Info.IsFloatingPoint, SAMPLEFORMAT_UINT, SAMPLEFORMAT_IEEEFP);
      PlanarConfig := PLANARCONFIG_CONTIG;
      CompressionScheme := Compressions[FCompression];

      // Check if selected compression scheme can be used for current image
      CompressionMismatch := (CompressionScheme = COMPRESSION_JPEG) and ((BitsPerSample <> 8) or
        not (SamplesPerPixel in [1, 3]) or Info.IsIndexed or Info.IsFloatingPoint);
      CompressionMismatch := CompressionMismatch or ((CompressionScheme = COMPRESSION_CCITTFAX4) and (Info.Format <> ifBinary));
      if CompressionMismatch then
        CompressionScheme := COMPRESSION_LZW;
      // If we have some compression scheme selected and it's not Fax then select it automatically - better comp ratios!
      if (Info.Format = ifBinary) and (CompressionScheme <> COMPRESSION_NONE) and (CompressionScheme <> COMPRESSION_CCITTFAX4) then
        CompressionScheme := COMPRESSION_CCITTFAX4;

      RowsPerStrip := TIFFDefaultStripSize(Tiff, Height);
      if Info.IsIndexed then
        Photometric := PHOTOMETRIC_PALETTE
      else if (Info.HasGrayChannel) or (Info.ChannelCount = 1) then
        Photometric := PHOTOMETRIC_MINISBLACK
      else
        Photometric := PHOTOMETRIC_RGB;

      // Write tags
      TIFFSetField(Tiff, TIFFTAG_IMAGEWIDTH, Width);
      TIFFSetField(Tiff, TIFFTAG_IMAGELENGTH, Height);
      TIFFSetField(Tiff, TIFFTAG_PHOTOMETRIC, Photometric);
      TIFFSetField(Tiff, TIFFTAG_PLANARCONFIG, PlanarConfig);
      TIFFSetField(Tiff, TIFFTAG_ORIENTATION, Orientation);
      TIFFSetField(Tiff, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
      TIFFSetField(Tiff, TIFFTAG_SAMPLESPERPIXEL, SamplesPerPixel);
      TIFFSetField(Tiff, TIFFTAG_SAMPLEFORMAT, SampleFormat);
      TIFFSetField(Tiff, TIFFTAG_COMPRESSION, CompressionScheme);
      if CompressionScheme = COMPRESSION_JPEG then
        TIFFSetField(Tiff, TIFFTAG_JPEGQUALITY, FJpegQuality);
      TIFFSetField(Tiff, TIFFTAG_ROWSPERSTRIP, RowsPerStrip);
      // Save supported metadata
      SaveMetadata(Tiff, I);

      if Format = ifIndex8 then
      begin
        // Set palette for indexed images
        for J := 0 to 255 do
        with ImageToSave.Palette[J] do
        begin
          Red[J].High := R;
          Green[J].High := G;
          Blue[J].High := B;
        end;
        TIFFSetField(Tiff, TIFFTAG_COLORMAP, @Red[0], @Green[0], @Blue[0]);
      end;

      ScanLineSize := Info.GetPixelsSize(Info.Format, Width, 1);

      if Photometric = PHOTOMETRIC_RGB then
        SwapChannels(ImageToSave, ChannelRed, ChannelBlue);
      // Write image scanlines and then directory for current image
      for J := 0 to Height - 1 do
        TIFFWriteScanline(Tiff, @PByteArray(Bits)[J * ScanLineSize], J, 0);
      if Info.ChannelCount > 1 then
        SwapChannels(ImageToSave, ChannelRed, ChannelBlue);

      TIFFWriteDirectory(Tiff);
    finally
      if MustBeFreed then
        FreeImage(ImageToSave);
    end;
  end;

  TIFFClose(Tiff);
  Result := True;
end;

procedure TTiffLibFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.RBSwapFormat in GetSupportedFormats then
    ConvFormat := Info.RBSwapFormat
  else if Info.IsFloatingPoint then
    ConvFormat :=  IffFormat(Info.ChannelCount = 1, ifR32F, ifA32R32G32B32F)
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray32)
  else
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

  ConvertImage(Image, ConvFormat);
end;

initialization
{$IFDEF USE_DYN_LIB}
  // If using dynamic library only register the format if
  // the library loads successfully.
  if LibTiffDynLib.LoadTiffLibrary then
{$ENDIF}
  RegisterImageFileFormat(TTiffLibFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.3 ----------------------------------------------------
    - Lot more platforms than just 32bit Delphi supported now.
    - Workaround for problem on 64bit Linux where thandle_t in libtiff is
      still 32bit so it cannot be used to pass pointers (for IO functions).
    - Support for libtiff as DLL/SO instead of linking object files to exe.
      Useful for platforms like Linux where libtiff is already installed
      most of the time (and exe could be make smaller not linking the objects).
    - Removed problematic append mode.
    - Renamed and refactored to be based on common Tiff base class
      (for shared stuff between other Tiff implementations (WIC, Quartz)).

  -- 0.77.1 ----------------------------------------------------
    - Renamed unit to ImagingLibTiffDelphi since there will be more
      Tiff implementations in the future, cleaned up interface units
      and obj file a little bit.
    - Updated LibTiff to version 3.9.4 and added EXIF tag support.
    - Added TIFF Append mode: when saving existing files are not
      overwritten but images are appended to TIFF instead.
    - Images in ifBinary format are now supported for loading/saving
      (optional Group 4 fax encoding added).
    - PHOTOMETRIC_MINISWHITE is now properly read as Grayscale/Binary
      instead of using unefficient RGBA interface.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Fix: All pages of multipage TIFF were loaded even when
      OnlyFirstLevel was True.
    - Loading and saving of physical resolution metadata.
    - Unicode compatibility fixes in LibTiffDelphi.
    - Added Jpeg compression quality setting.

  -- 0.24.3 Changes/Bug Fixes ---------------------------------
    - Fixed bug in loading and saving of 2 channel images - Imaging
      tried to swap R and B channels here.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added TIFF loading and saving.
    - Unit created and initial code added.
}

end.
